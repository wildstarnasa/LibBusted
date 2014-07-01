local MAJOR,MINOR = "Lib:Busted-2.0", 5
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion or 0) >= MINOR then
    return -- no upgrade needed
end
-- Set a reference to the actual package or create an empty table
local LibBusted = APkg and APkg.tPackage or {}
local s -- Olivine-Say
local busted
local pretty

local BustedTests = setmetatable({}, { __index = function(tbl, key) tbl[key] = {} return tbl[key] end })

local tLibError = Apollo.GetPackage("Gemini:LibError-1.0")
local fnErrorHandler = tLibError and tLibError.tPackage and tLibError.tPackage.Error or Print

-- first load the submodules
local function loadModule(dir, folder, file)
    local func = assert(loadfile(dir..folder.."\\"..file..".lua"))
    if func then
        return xpcall(func, fnErrorHandler)
    end
end

-- This gets the current directory of this file, so it also works when embedded
local strsub, strgsub, debug = string.sub, string.gsub, debug
local dir = string.sub(string.gsub(debug.getinfo(1).source, "^(.+[\\/])[^\\/]+$", "%1"), 2, -1)

loadModule(dir, "Say", "Say-1.0")

loadModule(dir, "Luassert", "util")
loadModule(dir, "Luassert", "spy")
loadModule(dir, "Luassert", "stub")
loadModule(dir, "Luassert", "mock")

loadModule(dir, "Mediator", "mediator")

loadModule(dir, "Penlight", "operator")
loadModule(dir, "Penlight", "utils")
loadModule(dir, "Penlight", "types")
loadModule(dir, "Penlight", "tablex")
loadModule(dir, "Penlight", "pretty")

loadModule(dir, "Busted", "core")

-------------------------------------------------------------------------------
--- Olivine-Labs Busted - Output Handler - Plain Terminal
-------------------------------------------------------------------------------
local function outputPlainTerminal(options, busted)
    -- options.language, options.deferPrint, options.suppressPending, options.verbose
    local handler = { }
    local tests = 0
    local successes = 0
    local failures = 0
    local pendings = 0

    local successString =  '+'
    local failureString =  '-'
    local pendingString = '.'

    local failureInfos = { }
    local pendingInfos = { }

    local startTime, endTime

    local getFullName = function(context)
        local parent = context.parent
        local names = { (context.name or context.descriptor) }

        while parent and (parent.name or parent.descriptor) and
                parent.descriptor ~= 'file' do

            table.insert(names, 1, parent.name or parent.descriptor)
            parent = busted.context.parent(parent)
        end

        return table.concat(names, ' ')
    end

    local pendingDescription = function(pending)
        local name = getFullName(pending)

        local string = '\n\n' .. s('output.pending') .. ': ' ..
            pending.elementTrace.short_src .. ' @ ' ..
            pending.elementTrace.currentline  ..
            '\n' .. name

        return string
    end

    local failureDescription = function(failure)
        local string =  s('output.failure') .. ': '
        if failure.elementTrace then
            string = string .. failure.elementTrace.short_src .. ' @ ' ..
            failure.elementTrace.currentline
        else
            string = string .. failure.debug
        end
        string = string .. '\n' .. getFullName(failure) .. '\n\n'

        if type(failure.message) == 'string' then
            string = string .. failure.message
        elseif failure.message == nil then
            string = string .. 'Nil error'
        else
            string = string .. pretty.write(failure.message)
        end

        if options.verbose then
            string = string .. failure.debug.traceback
        end

        return string
    end

    local statusString = function(successes, failures, pendings, ms)
        local successString = s('output.success_plural')
        local failureString = s('output.failure_plural')
        local pendingString = s('output.pending_plural')

        if successes == 0 then
            successString = s('output.success_zero')
        elseif successes == 1 then
            successString = s('output.success_single')
        end

        if failures == 0 then
            failureString = s('output.failure_zero')
        elseif failures == 1 then
            failureString = s('output.failure_single')
        end

        if pendings == 0 then
            pendingString = s('output.pending_zero')
        elseif pendings == 1 then
            pendingString = s('output.pending_single')
        end

        local formattedTime = ('%.6f'):format(ms):gsub('([0-9])0+$', '%1')

        return successes .. ' ' .. successString .. ' / ' ..
            failures .. ' ' .. failureString .. ' / ' ..
            pendings .. ' ' .. pendingString .. ' : ' ..
            formattedTime .. ' ' .. s('output.seconds')
    end

    handler.testStart = function(name, parent)
        tests = tests + 1

        return nil, true
    end

    handler.testEnd = function(element, parent, status, debug)
        local string = successString

        if status == 'success' then
            successes = successes + 1
        elseif status == 'pending' then
            if not options.suppressPending then
            Print(pendingString)
            pendings = pendings + 1
            table.insert(pendingInfos, {
                name = element.name,
                elementTrace = element.trace,
                parent = parent
            })
            end
        elseif status == 'failure' then
            string = failureString
            failures = failures + 1
        end

        if not options.deferPrint then
            Print(string)
            --io.write(string)
        end

        return nil, true
    end

    handler.pending = function(element, parent, message, debug)
        return nil, true
    end

    handler.fileStart = function(name, parent)
        return nil, true
    end

    handler.fileEnd = function(name, parent)
        return nil, true
    end

    handler.suiteStart = function(name, parent)
        startTime = os.clock()
        return nil, true
    end

    handler.suiteEnd = function(name, parent)
        endTime = os.clock()
        -- print an extra newline of defer print
        if not options.deferPrint then
            Print('')
        end

        Print(statusString(successes, failures, pendings, endTime - startTime, {}))

        if #pendingInfos > 0 then print('') end
        for i, pending in pairs(pendingInfos) do
            Print(pendingDescription(pending))
        end

        if #failureInfos > 0 then print('') end
        for i, err in pairs(failureInfos) do
            Print(failureDescription(err))
        end

        tests, successes, failures, pendings = 0, 0, 0, 0

        failureInfos, pendingInfos = {}, {}

        return nil, true
    end

    handler.error = function(element, parent, message, debug)
        table.insert(failureInfos, {
            elementTrace = element.trace,
            name = element.name,
            descriptor = element.descriptor,
            message = message,
            debug = debug,
            parent = parent
        })

        return nil, true
    end

    return handler
end

-------------------------------------------------------------------------------
--- Olivine-Labs Busted - Sound Output
-------------------------------------------------------------------------------

local function outputSound(options, busted)
    local handler = {}

    local isFailure = false

    handler.testEnd = function(element, parent, status)
        if status == 'failure' then
            isFailure = true
        end

        return nil, true
    end

    handler.suiteEnd = function(name, parent)
        local messages, soundNum

        if isFailure then
            messages = busted.failure_messages
            soundNum = Sound.PlayUIChallengeQuestFailed
        else
            messages = busted.success_messages
            soundNum = Sound.PlayUIChallengeQuestComplete
        end

        Sound.Play(soundNum)
        Print(messages[math.random(1, #messages)])

        isFailure = false

        return nil, true
    end

    handler.error = function(element, parent, message, debug)
        isFailure = true

        return nil, true
    end

    return handler
end

-------------------------------------------------------------------------------
--- Olivine-Labs Busted - Lua Test File
-------------------------------------------------------------------------------

local LuaTests
do
    local ret = {}
    local getTrace =  function(filename, info)
        local index = info.traceback:find('\n%s*%[C]')
        info.traceback = info.traceback:sub(1, index)
        return info, false
    end

    ret.load = function(busted, filename)
        local file
        local success, err = pcall(function()
            file, err = loadfile(filename)

            if not file then
                busted.publish({ 'error', 'file' }, filename, nil, nil, err)
            end
        end)

        if not success then
            busted.publish({ 'error', 'file' }, filename, nil, nil, err)
        end

        return file, getTrace
    end
    LuaTests = ret
end

-------------------------------------------------------------------------------
--- Olivine-Labs Busted - v2
-------------------------------------------------------------------------------
local function ExecuteTests()
    local register = busted.Register
    busted.Register = function() end

    busted.publish({ 'suite', 'start' })
    busted.execute()
    busted.publish({ 'suite', 'end' })
    busted.context.reset()

    busted.Register = register
end

local function RunTest(self, strTest)
    local strFile = BustedTests[self][strTest]
    if not strFile then return end
    local testFile, getTrace = LuaTests.load(busted, strFile)

    if testFile then
        local file = setmetatable({
            getTrace = getTrace
        }, {
            __call = testFile
        })
        busted.getEnvironment().set('self', self)
        busted.executors.file(strTest, file)
        ExecuteTests()
    end
end

local function RunTests(self, config)
    local bLoadedTests
    for k,v in pairs(BustedTests[self]) do
        local testFile, getTrace = LuaTests.load(busted, v)

        if testFile then
            bLoadedTests = true
            local file = setmetatable({
                getTrace = getTrace
            }, {
                __call = testFile
            })
            busted.executors.file(k, file)
        end
    end
    if bLoadedTests then
        busted.getEnvironment().set('self', self)
        ExecuteTests()
    end
end

local function IterateTests(self)
    return pairs(BustedTests[self])
end

local tMixins = {
    RunTest = RunTest,
    RunTests = RunTests,
    IterateTests = IterateTests,
}

function LibBusted:Register(oAddon)
    local bFoundTests
    local strAssetFolder = Apollo.GetAssetFolder()
    local tocXML = XmlDoc.CreateFromFile("toc.xml"):ToTable()
    for k,v in pairs(tocXML) do
        if v.__XmlNode == "Test" then
            BustedTests[oAddon][v.Name] = strAssetFolder .. "\\" .. v.File
            bFoundTests = true
        end
    end

    if bFoundTests then
        for k, v in pairs(tMixins) do
            oAddon[k] = v
        end
    end
end

function LibBusted:OnDependencyError(strDep, strError)
    return false
end

function LibBusted:OnLoad()
    s = Apollo.GetPackage("Olivine:Say-1.0").tPackage
    pretty = Apollo.GetPackage("Lib:Penlight:Pretty-1.0").tPackage
    busted = Apollo.GetPackaget("Olivine:Busted:Core-2.0").tPackage

    -- Set up output handler to listen to events TODO: Allow config, support tags
    local outputHandlerOptions = {
        verbose = true,
        suppressPending = true,
        deferPrint = false,
    }

    local outputHandler = outputPlainTerminal(outputHandlerOptions, busted) -- (only choice for now)

    busted.subscribe({ 'test', 'start' }, outputHandler.testStart)
    busted.subscribe({ 'test', 'end' }, outputHandler.testEnd)
    busted.subscribe({ 'file', 'start' }, outputHandler.fileStart)
    busted.subscribe({ 'file', 'end' }, outputHandler.fileEnd)
    busted.subscribe({ 'suite', 'start' }, outputHandler.suiteStart)
    busted.subscribe({ 'suite', 'end' }, outputHandler.suiteEnd)
    busted.subscribe({ 'error' }, outputHandler.error)

    if true then -- TODO: Config
        local sound = outputSound(outputHandlerOptions, busted)
        busted.subscribe({ 'test', 'end' }, sound.testEnd)
        busted.subscribe({ 'suite', 'end' }, sound.suiteEnd)
        busted.subscribe({ 'error' }, sound.error)
    end
end

if _TESTRUNNER then
    function LibBusted:RunAllTests()
        for addon, tests in pairs(BustedTests) do
            addon:RunTests()
        end
    end
end

local tDependencies = {
    "Lib:Assert-1.0",
    "Olivine:Say-1.0",
    "Olivine:Busted:Core-2.0",
}

Apollo.RegisterPackage(LibBusted, MAJOR, MINOR, tDependencies)
