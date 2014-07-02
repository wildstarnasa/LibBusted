local MAJOR, MINOR = "Olivine:Busted:Core-2.0", 1
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion or 0) >= MINOR then
    return
end

-------------------------------------------------------------------------------
--- Olivine-Labs Busted-Core
-------------------------------------------------------------------------------
local busted = APkg and APkg.tPackage or {}

local tLibError = Apollo.GetPackage("Gemini:LibError-1.0")
local fnErrorHandler = tLibError and tLibError.tPackage and tLibError.tPackage.Error or Print

-- first load the submodules
local function loadModule(dir, file)
    local func = assert(loadfile(dir..file..".lua"))
    if func then
        -- Does not handle multiple return values, but these are not using that.
        local bSuccess, retVal = xpcall(func, fnErrorHandler)
        return retVal
    end
end

-- This gets the current directory of this file, so it also works when embedded
local strsub, strgsub, debug = string.sub, string.gsub, debug
local dir = string.sub(string.gsub(debug.getinfo(1).source, "^(.+[\\/])[^\\/]+$", "%1"), 2, -1)

loadModule(dir, "done")

local mediator
busted.version = '2.0-1'

local LibEnvironment
local environment

busted.getEnvironment = function()
    return environment
end

busted.executors = {}
local executors = {}

busted.getTrace = function(element, level, msg)
    level = level or  3

    local info = debug.getinfo(level, 'Sl')
    info.traceback = debug.traceback("", level)
    info.message = msg

    local file = busted.getFile(element, name)
    return file.getTrace(name, info)
end

function busted.publish(...)
    return mediator:publish(...)
end

function busted.subscribe(...)
    return mediator:subscribe(...)
end

function busted.getFile(element)
    local current, parent = element, busted.context.parent(element)

    while parent do
        if parent.file then
            local file = parent.file[1]
            return {
                name = file.name,
                getTrace = file.run.getTrace
            }
        end

        if parent.descriptor == 'file' then
            return {
                name = parent.name,
                getTrace = parent.run.getTrace
            }
        end

        parent = busted.context.parent(parent)
    end

    return parent
end

function busted.safe(descriptor, run, element, setenv)
    if setenv and (type(run) == 'function' or getmetatable(run).__call) then
        -- prioritize __call if it exists, like in files
        environment.wrap(getmetatable(run).__call or run)
    end

    busted.context.push(element)
    local trace, message

    local ret = { xpcall(run, function(msg)
        message = msg
        trace = busted.getTrace(element, 3, msg)
    end) }

    if not ret[1] then
        busted.publish({ 'error', descriptor }, element, busted.context.parent(element), message, trace)
    end

    busted.context.pop()
    return unpack(ret)
end

function busted.register(descriptor, executor)
    executors[descriptor] = executor

    local publisher = function(name, fn)
        if not fn and type(name) == 'function' then
            fn = name
            name = nil
        end

        local trace

        if descriptor ~= 'file' then
            trace = busted.getTrace(busted.context.get(), 3, name)
        end

        busted.publish({ 'register', descriptor }, name, fn, trace)
    end

    busted.executors[descriptor] = publisher
    environment.set(descriptor, publisher)

    busted.subscribe({ 'register', descriptor }, function(name, fn, trace)
        local ctx = busted.context.get()
        local plugin = {
            descriptor = descriptor,
            name = name,
            run = fn,
            trace = trace
        }

        busted.context.attach(plugin)

        if not ctx[descriptor] then
            ctx[descriptor] = { plugin }
        else
            ctx[descriptor][#ctx[descriptor]+1] = plugin
        end
    end)
end

function busted.execute(current)
    if not current then current = busted.context.get() end
    for _, v in pairs(busted.context.children(current)) do
        local executor = executors[v.descriptor]
        if executor then
            busted.safe(v.descriptor, function() return executor(v) end, v)
        end
    end
end

--------------------------------------------------------------------------------
--- Wildstar Specific code
--------------------------------------------------------------------------------

local ktLocales = {
    [1] = "enUS",
    [2] = "deDE",
    [3] = "frFR",
    [4] = "koKR",
}

-- check the locale.languageId console variable set by the launcher
-- if not found, default to enUS
local function GetLocale()
    local strCancel = Apollo.GetString(1)

    -- German
    if strCancel == "Abbrechen" then
        return ktLocales[2]
    end

    -- French
    if strCancel == "Annuler" then
        return ktLocales[3]
    end

    -- Other
    return ktLocales[1]
    -- return ktLocales[(Apollo.GetConsoleVariable("locale.languageId") or 1)]
end

function busted:OnLoad()
    local s = Apollo.GetPackage("Olivine:Say-1.0").tPackage

    local strLocale = GetLocale()
    if strLocale == "enUS" then
        s:set_namespace('en')

        -- 'Pending: test.lua @ 12 \n description
        s:set('output.pending', 'Pending')
        s:set('output.failure', 'Failure')
        s:set('output.success', 'Success')

        s:set('output.pending_plural', 'pending')
        s:set('output.failure_plural', 'failures')
        s:set('output.success_plural', 'successes')

        s:set('output.pending_zero', 'pending')
        s:set('output.failure_zero', 'failures')
        s:set('output.success_zero', 'successes')

        s:set('output.pending_single', 'pending')
        s:set('output.failure_single', 'failure')
        s:set('output.success_single', 'success')

        s:set('output.seconds', 'seconds')

        -- definitions following are not used within the 'say' namespace
        self.failure_messages = {
            'You have %d busted specs',
            'Your specs are busted',
            'Your code is bad and you should feel bad',
            'Your code is in the Danger Zone',
            'Strange game. The only way to win is not to test',
            'My grandmother wrote better specs on a 3 86',
            'Every time there\'s a failure, drink another beer',
            'Feels bad man',
        }

        self.success_messages = {
            'Aww yeah, passing specs',
            'Doesn\'t matter, had specs',
            'Feels good, man',
            'Great success',
            'Tests pass, drink another beer',
        }
    elseif strLocale == "deDE" then
        s:set_namespace('de')

        -- 'Pending: test.lua @ 12 \n description
        s:set('output.pending', 'Noch nicht erledigt')
        s:set('output.failure', 'Fehlgeschlagen')
        s:set('output.success', 'Erfolgreich')

        s:set('output.pending_plural', 'übersprungen')
        s:set('output.failure_plural', 'fehlgeschlagen')
        s:set('output.success_plural', 'erfolgreich')

        s:set('output.pending_zero', 'übersprungen')
        s:set('output.failure_zero', 'fehlgeschlagen')
        s:set('output.success_zero', 'erfolgreich')

        s:set('output.pending_single', 'übersprungen')
        s:set('output.failure_single', 'fehlgeschlagen')
        s:set('output.success_single', 'erfolgreich')

        s:set('output.seconds', 'Sekunden')

        -- definitions following are not used within the 'say' namespace
        self.failure_messages = {
            'Du hast %d kaputte Tests.',
            'Deine Tests sind kaputt.',
            'Dein Code ist schlecht; du solltest dich schlecht fühlen.',
            'Dein Code befindet sich in der Gefahrenzone.',
            'Ein seltsames Spiel. Der einzig gewinnbringende Zug ist nicht zu testen.',
            'Meine Großmutter hat auf einem 386er bessere Tests geschrieben.',
            'Immer wenn ein Test fehlschlägt, stirbt ein kleines Kätzchen.',
            'Das fühlt sich schlecht an, oder?'
        }
        self.success_messages = {
            'Yeah, die Tests laufen durch.',
            'Fühlt sich gut an, oder?',
            'Großartig!',
            'Tests sind durchgelaufen, Zeit für ein Bier.',
        }
    elseif strLocale == "frFR" then
        s:set_namespace('fr')

        -- 'Pending: test.lua @ 12 \n description
        s:set('output.pending', 'En attente')
        s:set('output.failure', 'Echec')
        s:set('output.success', 'Reussite')

        s:set('output.pending_plural', 'en attente')
        s:set('output.failure_plural', 'echecs')
        s:set('output.success_plural', 'reussites')

        s:set('output.pending_zero', 'en attente')
        s:set('output.failure_zero', 'echec')
        s:set('output.success_zero', 'reussite')

        s:set('output.pending_single', 'en attente')
        s:set('output.failure_single', 'echec')
        s:set('output.success_single', 'reussite')

        s:set('output.seconds', 'secondes')

        -- definitions following are not used within the 'say' namespace
        self.failure_messages = {
            'Vous avez %d test(s) qui a/ont echoue(s)',
            'Vos tests ont echoue.',
            'Votre code source est mauvais et vous devrez vous sentir mal',
            'Vous avez un code source de Destruction Massive',
            'Jeu plutot etrange game. Le seul moyen de gagner est de ne pas l\'essayer',
            'Meme ma grand-mere ecrivait de meilleurs tests sur un PIII x86',
            'A chaque erreur, prenez une biere',
            'Ca craint, mon pote'
        }
        self.success_messages = {
            'Oh yeah, tests reussis',
            'Pas grave, y\'a eu du succes',
            'C\'est du bon, mon pote. Que du bon!',
            'Reussi, haut la main!',
            'Test reussi. Un de plus. Offre toi une biere, sur mon compte!',
        }
    end

    mediator = Apollo.GetPackage("Olivine:Mediator-1.0").tPackage()

    local rootFunc = loadModule(dir, "context")
    busted.context = rootFunc().ref()

    local envBuilder = loadModule(dir, "environment")
    environment = envBuilder(busted.context)

    local mockLib = Apollo.GetPackage("Olivine:Luassert:Mock-1.0").tPackage

    environment.set('mock', mockLib.mock)
    environment.set('unmock', mockLib.unmock)
    environment.set('spy', Apollo.GetPackage("Olivine:Luassert:Spy-1.0").tPackage)
    environment.set('stub', Apollo.GetPackage("Olivine:Luassert:Stub-1.0").tPackage)
    environment.set('s', s)

    local initFunc = loadModule(dir, "init")
    initFunc(busted)
end

local tDependencies = {
    "Olivine:Mediator-1.0",
    "Olivine:Luassert:Mock-1.0",
    "Olivine:Luassert:Spy-1.0",
    "Olivine:Luassert:Stub-1.0",
    "Olivine:Say-1.0",
}

Apollo.RegisterPackage(busted, MAJOR, MINOR, tDependencies)
