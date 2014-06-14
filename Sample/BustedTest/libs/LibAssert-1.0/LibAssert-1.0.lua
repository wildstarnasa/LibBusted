local MAJOR,MINOR = "Lib:Assert-1.0", 3
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion or 0) >= MINOR then
	return -- no upgrade needed
end
-- Set a reference to the actual package or create an empty table
local Lib = APkg and APkg.tPackage or {}

local oldAssert

-------------------------------------------------------------------------------
--- Olivine-Labs Say
-------------------------------------------------------------------------------

local SAY_MAJOR, SAY_MINOR = "Olivine:Say-1.0", 1
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(SAY_MAJOR)
-- Set a reference to the actual package or create an empty table
local s = APkg and APkg.tPackage or {}
-- If there was an older version loaded we need to see if this is newer
if not APkg or (APkg.nVersion or 0) < SAY_MINOR then
	do
		local registry = s.get_registry and s.get_registry() or { }
		local current_namespace
		local fallback_namespace

		s = {

			_COPYRIGHT   = "Copyright (c) 2012 Olivine Labs, LLC.",
			_DESCRIPTION = "A simple string key/value store for i18n or any other case where you want namespaced strings.",
			_VERSION     = "Say 1.2",

			set_namespace = function(self, namespace)
				current_namespace = namespace
				if not registry[current_namespace] then
					registry[current_namespace] = {}
				end
			end,

			set_fallback = function(self, namespace)
				fallback_namespace = namespace
				if not registry[fallback_namespace] then
					registry[fallback_namespace] = {}
				end
			end,

			set = function(self, key, value)
				registry[current_namespace][key] = value
			end,

			get_registry = function()
				return registry
			end,
		}

		local __meta = {
			__call = function(self, key, vars)
				vars = vars or {}

				local str = registry[current_namespace][key] or registry[fallback_namespace][key]

				if str == nil then
					return nil
				end
				str = tostring(str)
				local strings = {}

				for i,v in ipairs(vars) do
					table.insert(strings, tostring(v))
				end

				return #strings > 0 and str:format(unpack(strings)) or str
			end,

			__index = function(self, key)
				return registry[key]
			end
		}

		s:set_fallback('en')
		s:set_namespace('en')

		if _TEST then
			s._registry = registry -- force different name to make sure with _TEST behaves exactly as without _TEST
		end

		setmetatable(s, __meta)
	end
	Apollo.RegisterPackage(s, SAY_MAJOR, SAY_MINOR, {})
end

-------------------------------------------------------------------------------
--- Olivine-Labs util
-------------------------------------------------------------------------------

local util = {}
do
	function util.deepcompare(t1,t2,ignore_mt)
		local ty1 = type(t1)
		local ty2 = type(t2)
		if ty1 ~= ty2 then return false end
		-- non-table types can be directly compared
		if ty1 ~= 'table' then return t1 == t2 end
		local mt1 = debug.getmetatable(t1)
		local mt2 = debug.getmetatable(t2)
		-- would equality be determined by metatable __eq?
		if mt1 and mt1 == mt2 and mt1.__eq then
			-- then use that unless asked not to
			if not ignore_mt then return t1 == t2 end
		else -- we can skip the deep comparison below if t1 and t2 share identity
			if t1 == t2 then return true end
		end
		for k1,v1 in pairs(t1) do
			local v2 = t2[k1]
			if v2 == nil or not util.deepcompare(v1,v2) then return false end
		end
		for k2,_ in pairs(t2) do
			-- only check wether each element has a t1 counterpart, actual comparison
			-- has been done in first loop above
			if t1[k2] == nil then return false end
		end
		return true
	end

	-----------------------------------------------
	-- table.insert() replacement that respects nil values.
	-- The function will use table field 'n' as indicator of the
	-- table length, if not set, it will be added.
	-- @param t table into which to insert
	-- @param pos (optional) position in table where to insert. NOTE: not optional if you want to insert a nil-value!
	-- @param val value to insert
	-- @return No return values
	function util.tinsert(...)
		-- check optional POS value
		local args = {...}
		local c = select('#',...)
		local t = args[1]
		local pos = args[2]
		local val = args[3]
		if c < 3 then
			val = pos
			pos = nil
		end
		-- set length indicator n if not present (+1)
		t.n = (t.n or #t) + 1
		if not pos then
			pos = t.n
		elseif pos > t.n then
			-- out of our range
			t[pos] = val
			t.n = pos
		end
		-- shift everything up 1 pos
		for i = t.n, pos + 1, -1 do
			t[i]=t[i-1]
		end
		-- add element to be inserted
		t[pos] = val
	end
	-----------------------------------------------
	-- table.remove() replacement that respects nil values.
	-- The function will use table field 'n' as indicator of the
	-- table length, if not set, it will be added.
	-- @param t table from which to remove
	-- @param pos (optional) position in table to remove
	-- @return No return values
	function util.tremove(t, pos)
		-- set length indicator n if not present (+1)
		t.n = t.n or #t
		if not pos then
			pos = t.n
		elseif pos > t.n then
			-- out of our range
			t[pos] = nil
			return
		end
		-- shift everything up 1 pos
		for i = pos, t.n do
			t[i]=t[i+1]
		end
		-- set size, clean last
		t[t.n] = nil
		t.n = t.n - 1
	end

	-----------------------------------------------
	-- Checks an element to be callable.
	-- The type must either be a function or have a metatable
	-- containing an '__call' function.
	-- @param object element to inspect on being callable or not
	-- @return boolean, true if the object is callable
	function util.callable(object)
		return type(object) == "function" or type((debug.getmetatable(object) or {}).__call) == "function"
	end
end

-------------------------------------------------------------------------------
--- Olivine-Labs Luaassert.state
-------------------------------------------------------------------------------

-- exported module table
local astate
do
	-- maintains a state of the assert engine in a linked-list fashion
	-- records; formatters, parameters, spies and stubs

	local state_mt = {
				__call = function(self)
					self:revert()
				end }

	local nilvalue = {} -- unique ID to refer to nil values for parameters

	-- will hold the current state
	local current

	-- exported module table
	local state = {}

	------------------------------------------------------
	-- Reverts to a (specific) snapshot.
	-- @param self (optional) the snapshot to revert to. If not provided, it will revert to the last snapshot.
	state.revert = function(self)
		if not self then
			-- no snapshot given, so move 1 up
			self = current
			if not self.previous then
				-- top of list, no previous one, nothing to do
				return
			end
		end
		if getmetatable(self) ~= state_mt then error("Value provided is not a valid snapshot", 2) end
		
		if self.next then
			self.next:revert()
		end
		-- revert formatters in 'last'
		self.formatters = {}
		-- revert parameters in 'last'
		self.parameters = {}
		-- revert spies/stubs in 'last'
		while self.spies[1] do
			self.spies[1]:revert()
			table.remove(self.spies, 1)
		end
		setmetatable(self, nil) -- invalidate as a snapshot
		current = self.previous
		current.next = nil
	end

	------------------------------------------------------
	-- Creates a new snapshot.
	-- @return snapshot table
	state.snapshot = function()
		local s = current
		local new = setmetatable ({
			formatters = {},
			parameters = {},
			spies = {},
			previous = current,
			revert = state.revert,
		}, state_mt)
		if current then current.next = new end
		current = new
		return current
	end


	--  FORMATTERS
	state.add_formatter = function(callback)
		table.insert(current.formatters, 1, callback)
	end

	state.remove_formatter = function(callback, s)
		s = s or current
		for i, v in ipairs(s.formatters) do
			if v == fmtr then
				table.remove(s.formatters, i)
				break
			end
		end
		-- wasn't found, so traverse up 1 state
		if s.previous then
			state.remove_formatter(callback, s.previous)
		end
	end

	state.format_argument = function(val, s)
		s = s or current
		for _, fmt in ipairs(s.formatters) do
			local valfmt = fmt(val)
			if valfmt ~= nil then return valfmt end
		end
		-- nothing found, check snapshot 1 up in list
		if s.previous then
			return state.format_argument(val, s.previous)
		end
		return nil -- end of list, couldn't format
	end


	--  PARAMETERS
	state.set_parameter = function(name, value)
		if value == nil then value = nilvalue end
		current.parameters[name] = value
	end

	state.get_parameter = function(name, s)
		s = s or current
		local val = s.parameters[name]
		if val == nil and s.previous then
			-- not found, so check 1 up in list
			return state.get_parameter(name, s.previous)
		end
		if val ~= nilvalue then
			return val
		end
		return nil
	end

	--  SPIES / STUBS
	state.add_spy = function(spy)
		table.insert(current.spies, 1, spy)
	end

	state.snapshot()  -- create initial state

	astate = state
end

-------------------------------------------------------------------------------
--- Olivine-Labs Luaassert.assert
-------------------------------------------------------------------------------

do
	local obj   -- the returned module table

	-- list of namespaces
	local namespace = {}

	local errorlevel = function()
		-- find the first level, not defined in the same file as this
		-- code file to properly report the error
		local level = 1
		local info = debug.getinfo(level)
		local thisfile = (info or {}).source
		while thisfile and thisfile == (info or {}).source do
			level = level + 1
			info = debug.getinfo(level)
		end
		if level > 1 then level = level - 1 end -- deduct call to errorlevel() itself
		return level
	end

	local function extract_keys(assert_string)
		-- get a list of token separated by _
		local tokens = {}
		for token in assert_string:lower():gmatch('[^_]+') do
			table.insert(tokens, token)
		end

		-- find valid keys by coalescing tokens as needed, starting from the end
		local keys = {}
		local key = nil
		for i = #tokens, 1, -1 do
			local token = tokens[i]
			key = key and (token .. '_' .. key) or token
			if namespace.modifier[key] or namespace.assertion[key] then
				table.insert(keys, 1, key)
				key = nil
			end
		end

		-- if there's anything left we didn't recognize it
		if key then
			error("luassert: unknown modifier/assertion: '" .. key .."'", errorlevel())
		end

		return keys
	end

	local __assertion_meta = {
		__call = function(self, ...)
			local state = self.state
			local arguments = {...}
			arguments.n = select('#',...)  -- add argument count for trailing nils
			local val = self.callback(state, arguments)
			local data_type = type(val)

			if data_type == "boolean" then
				if val ~= state.mod then
					if state.mod then
						error(s(self.positive_message, obj:format(arguments)) or "assertion failed!", errorlevel())
					else
						error(s(self.negative_message, obj:format(arguments)) or "assertion failed!", errorlevel())
					end
				else
					return state
				end
			end
			return val
		end
	}

	local __state_meta = {

		__call = function(self, payload, callback)
			self.payload = payload or rawget(self, "payload")
			if callback then callback(self) end
			return self
		end,

		__index = function(self, key)
			local keys = extract_keys(key)

			-- execute modifiers and assertions
			local ret = nil
			for _, key in ipairs(keys) do
				if namespace.modifier[key] then
					namespace.modifier[key].state = self
					ret = self(nil, namespace.modifier[key])
				elseif namespace.assertion[key] then
					namespace.assertion[key].state = self
					ret = namespace.assertion[key]
				end
			end
			return ret
		end
	}

	obj = {
		state = function() return setmetatable({mod=true, payload=nil}, __state_meta) end,

		-- registers a function in namespace
		register = function(self, nspace, name, callback, positive_message, negative_message)
			-- register
			local lowername = name:lower()
			if not namespace[nspace] then
				namespace[nspace] = {}
			end
			namespace[nspace][lowername] = setmetatable({
				callback = callback,
				name = lowername,
				positive_message=positive_message,
				negative_message=negative_message
			}, __assertion_meta)
		end,

		-- registers a formatter
		-- a formatter takes a single argument, and converts it to a string, or returns nil if it cannot format the argument
		add_formatter = function(self, callback)
			astate.add_formatter(callback)
		end,

		-- unregisters a formatter
		remove_formatter = function(self, fmtr)
			astate.remove_formatter(fmtr)
		end,

		format = function(self, args)
			-- args.n specifies the number of arguments in case of 'trailing nil' arguments which get lost
			local nofmt = args.nofmt or {}  -- arguments in this list should not be formatted
			for i = 1, (args.n or #args) do -- cannot use pairs because table might have nils
				if not nofmt[i] then
					local val = args[i]
					local valfmt = astate.format_argument(val)
					if valfmt == nil then valfmt = tostring(val) end -- no formatter found
					args[i] = valfmt
				end
			end
			return args
		end,

		set_parameter = function(self, name, value)
			astate.set_parameter(name, value)
		end,
		
		get_parameter = function(self, name)
			return astate.get_parameter(name)
		end,  
		
		add_spy = function(self, spy)
			astate.add_spy(spy)
		end,
		
		snapshot = function(self)
			return astate.snapshot()
		end,
	}

	local __meta = {

		__call = function(self, bool, message, ...)
			if not bool then
				error(message or "assertion failed!", 2)
			end
			return bool , message , ...
		end,

		__index = function(self, key)
			return rawget(self, key) or self.state()[key]
		end,

	}

	Lib = setmetatable(obj, __meta)
end

-------------------------------------------------
--- Olivine-Labs assertions
-------------------------------------------------
-- module will not return anything, only register assertions with the main assert engine

-- assertions take 2 parameters;
-- 1) state
-- 2) arguments list. The list has a member 'n' with the argument count to check for trailing nils
-- returns; boolean; whether assertion passed

local assert = Lib
do
	local function unique(state, arguments)
		local list = arguments[1]
		local deep = arguments[2]
		for k,v in pairs(list) do
			for k2, v2 in pairs(list) do
				if k ~= k2 then
					if deep and util.deepcompare(v, v2, true) then
						return false
					else
						if v == v2 then
							return false
						end
					end
				end
			end
		end
		return true
	end

	local function equals(state, arguments)
		local argcnt = arguments.n
		assert(argcnt > 1, s("assertion.internal.argtolittle", { "equals", 2, tostring(argcnt) }))
		for i = 2,argcnt  do
			if arguments[1] ~= arguments[i] then
				-- switch arguments for proper output message
				util.tinsert(arguments, 1, arguments[i])
				util.tremove(arguments, i + 1)
				return false
			end
		end
		return true
	end

	local function same(state, arguments)
		local argcnt = arguments.n
		assert(argcnt > 1, s("assertion.internal.argtolittle", { "same", 2, tostring(argcnt) }))
		local prev = nil
		for i = 2,argcnt  do
			if type(arguments[1]) == 'table' and type(arguments[i]) == 'table' then
				if not util.deepcompare(arguments[1], arguments[i], true) then
					-- switch arguments for proper output message
					util.tinsert(arguments, 1, arguments[i])
					util.tremove(arguments, i + 1)
					return false
				end
			else
				if arguments[1] ~= arguments[i] then
					-- switch arguments for proper output message
					util.tinsert(arguments, 1, arguments[i])
					util.tremove(arguments, i + 1)
					return false
				end
			end
		end
		return true
	end

	local function truthy(state, arguments)
		return arguments[1] ~= false and arguments[1] ~= nil
	end

	local function falsy(state, arguments)
		return not truthy(state, arguments)
	end

	local function has_error(state, arguments)
		local func = arguments[1]
		local err_expected = arguments[2]
		
		assert(util.callable(func), s("assertion.internal.badargtype", { "error", "function, or callable object", type(func) }))
		local err_actual = nil
		--must swap error functions to get the actual error message
		local old_error = error
		error = function(err)
			err_actual = err
			return old_error(err)
		end
		local status = pcall(func)
		error = old_error
		local val = not status and (err_expected == nil or same(state, {err_expected, err_actual, ["n"] = 2}))

		return val
	end

	local function is_true(state, arguments)
		util.tinsert(arguments, 2, true)
		arguments.n = arguments.n + 1
		return arguments[1] == arguments[2]
	end

	local function is_false(state, arguments)
		util.tinsert(arguments, 2, false)
		arguments.n = arguments.n + 1
		return arguments[1] == arguments[2]
	end

	local function is_type(state, arguments, etype)
		util.tinsert(arguments, 2, "type " .. etype)
		arguments.nofmt = arguments.nofmt or {}
		arguments.nofmt[2] = true
		arguments.n = arguments.n + 1
		return arguments.n > 1 and type(arguments[1]) == etype
	end

	local function returned_arguments(state, arguments)
		arguments[1] = tostring(arguments[1])
		arguments[2] = tostring(arguments.n - 1)
		arguments.nofmt = arguments.nofmt or {}
		arguments.nofmt[1] = true
		arguments.nofmt[2] = true
		if arguments.n < 2 then arguments.n = 2 end
		return arguments[1] == arguments[2]
	end

	local function is_boolean(state, arguments)  return is_type(state, arguments, "boolean")  end
	local function is_number(state, arguments)   return is_type(state, arguments, "number")   end
	local function is_string(state, arguments)   return is_type(state, arguments, "string")   end
	local function is_table(state, arguments)    return is_type(state, arguments, "table")    end
	local function is_nil(state, arguments)      return is_type(state, arguments, "nil")      end
	local function is_userdata(state, arguments) return is_type(state, arguments, "userdata") end
	local function is_function(state, arguments) return is_type(state, arguments, "function") end
	local function is_thread(state, arguments)   return is_type(state, arguments, "thread")   end

	assert:register("assertion", "true", is_true, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "false", is_false, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "boolean", is_boolean, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "number", is_number, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "string", is_string, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "table", is_table, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "nil", is_nil, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "userdata", is_userdata, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "function", is_function, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "thread", is_thread, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "returned_arguments", returned_arguments, "assertion.returned_arguments.positive", "assertion.returned_arguments.negative")

	assert:register("assertion", "same", same, "assertion.same.positive", "assertion.same.negative")
	assert:register("assertion", "equals", equals, "assertion.equals.positive", "assertion.equals.negative")
	assert:register("assertion", "equal", equals, "assertion.equals.positive", "assertion.equals.negative")
	assert:register("assertion", "unique", unique, "assertion.unique.positive", "assertion.unique.negative")
	assert:register("assertion", "error", has_error, "assertion.error.positive", "assertion.error.negative")
	assert:register("assertion", "errors", has_error, "assertion.error.positive", "assertion.error.negative")
	assert:register("assertion", "truthy", truthy, "assertion.truthy.positive", "assertion.truthy.negative")
	assert:register("assertion", "falsy", falsy, "assertion.falsy.positive", "assertion.falsy.negative")
end

-------------------------------------------------------------------------------
--- Olivine-Labs modifiers
-------------------------------------------------------------------------------

do
	local function is(state)
		return state
	end

	local function is_not(state)
		state.mod = not state.mod
		return state
	end

	assert:register("modifier", "is", is)
	assert:register("modifier", "are", is)
	assert:register("modifier", "was", is)
	assert:register("modifier", "has", is)
	assert:register("modifier", "not", is_not)
	assert:register("modifier", "no", is_not)
end

-------------------------------------------------------------------------------
--- Olivine-Labs formatters
-------------------------------------------------------------------------------

do
	local function fmt_string(arg)
		if type(arg) == "string" then
			return string.format("(string) '%s'", arg)
		end
	end

	local function fmt_number(arg)
		if type(arg) == "number" then
			return string.format("(number) %s", tostring(arg))
		end
	end

	local function fmt_boolean(arg)
		if type(arg) == "boolean" then
			return string.format("(boolean) %s", tostring(arg))
		end
	end

	local function fmt_nil(arg)
		if type(arg) == "nil" then
			return "(nil)"
		end
	end

	local function fmt_table(arg)
		local tmax = assert:get_parameter("TableFormatLevel")
		local ft
		ft = function(t, l)
			local result = ""
			for k, v in pairs(t) do
				if type(v) == "table" then
					if l < tmax or tmax < 0 then
						result = result .. string.format(string.rep(" ",l * 2) .. "[%s] = {\n%s }\n", tostring(k), tostring(ft(v, l + 1):sub(1,-2)))
					else
						result = result .. string.format(string.rep(" ",l * 2) .. "[%s] = { ... more }\n", tostring(k))
					end
				else
					if type(v) == "string" then v = "'"..v.."'" end
					result = result .. string.format(string.rep(" ",l * 2) .. "[%s] = %s\n", tostring(k), tostring(v))
				end
			end
			return result
		end
		if type(arg) == "table" then
			local result
			if tmax == 0 then
				if next(arg) then
					result = "(table): { ... more }"
				else
					result = "(table): { }"
				end
			else
				result = "(table): {\n" .. ft(arg, 1):sub(1,-2) .. " }\n"
				result = result:gsub("{\n }\n", "{ }\n") -- cleanup empty tables
				result = result:sub(1,-2)                -- remove trailing newline
			end
			return result
		end
	end

	local function fmt_function(arg)
		if type(arg) == "function" then
			local debug_info = debug.getinfo(arg)
			return string.format("%s @ line %s in %s", tostring(arg), tostring(debug_info.linedefined), tostring(debug_info.source))
		end
	end

	local function fmt_userdata(arg)
		if type(arg) == "userdata" then
			return string.format("(userdata) '%s'", tostring(arg))
		end
	end

	assert:add_formatter(fmt_string)
	assert:add_formatter(fmt_number)
	assert:add_formatter(fmt_boolean)
	assert:add_formatter(fmt_nil)
	assert:add_formatter(fmt_table)
	assert:add_formatter(fmt_function)
	assert:add_formatter(fmt_userdata)
	-- Set default table display depth for table formatter
	assert:set_parameter("TableFormatLevel", 3)
end

-------------------------------------------------------------------------------
--- Wildstar related
-------------------------------------------------------------------------------

-- this locale lookup table is accurate for build 6512
local ktLocales = {
	[1] = "enUS",
	[2] = "deDE",
	[3] = "frFR",
	[4] = "koKR",
}
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
--  return ktLocales[(Apollo.GetConsoleVariable("locale.languageId") or 1)]
end

function Lib:OnSave()
	-- Apparently Wildstar likes crashing silently if assert is replaced during reloadui.
		-- Most likely the reference is torn down then checked (nil function reference)
		-- To prevent the crash we return assert to the original during OnSave
	_G.assert = oldAssert
end

function Lib:OnDependencyError(strDep, strError)
	return false
end

function Lib:OnLoad()
	oldAssert = _G.assert
	_G.assert = Lib

	local strLocale = GetLocale()

	if strLocale == "enUS" then
		s:set_namespace('en')

		s:set("assertion.same.positive", "Expected objects to be the same. Passed in:\n%s\nExpected:\n%s")
		s:set("assertion.same.negative", "Expected objects to not be the same. Passed in:\n%s\nDid not expect:\n%s")

		s:set("assertion.equals.positive", "Expected objects to be equal. Passed in:\n%s\nExpected:\n%s")
		s:set("assertion.equals.negative", "Expected objects to not be equal. Passed in:\n%s\nDid not expect:\n%s")

		s:set("assertion.unique.positive", "Expected object to be unique:\n%s")
		s:set("assertion.unique.negative", "Expected object to not be unique:\n%s")

		s:set("assertion.error.positive", "Expected error to be thrown.")
		s:set("assertion.error.negative", "Expected error to not be thrown.\n%s")

		s:set("assertion.truthy.positive", "Expected to be truthy, but value was:\n%s")
		s:set("assertion.truthy.negative", "Expected to not be truthy, but value was:\n%s")

		s:set("assertion.falsy.positive", "Expected to be falsy, but value was:\n%s")
		s:set("assertion.falsy.negative", "Expected to not be falsy, but value was:\n%s")

		s:set("assertion.called.positive", "Expected to be called %s time(s), but was called %s time(s)")
		s:set("assertion.called.negative", "Expected not to be called exactly %s time(s), but it was.")

		s:set("assertion.called_with.positive", "Function was not called with the arguments")
		s:set("assertion.called_with.negative", "Function was called with the arguments")

		s:set("assertion.returned_arguments.positive", "Expected to be called with %s argument(s), but was called with %s")
		s:set("assertion.returned_arguments.negative", "Expected not to be called with %s argument(s), but was called with %s")

		-- errors
		s:set("assertion.internal.argtolittle", "the '%s' function requires a minimum of %s arguments, got: %s")
		s:set("assertion.internal.badargtype", "the '%s' function requires a %s as an argument, got: %s")
	elseif strLocal == "deDE" then
		s:set_namespace('de')

		s:set("assertion.same.positive", "Erwarte gleiche Objekte. Gegeben:\n%s\nErwartet:\n%s")
		s:set("assertion.same.negative", "Erwarte ungleiche Objekte. Gegeben:\n%s\nNicht erwartet:\n%s")

		s:set("assertion.equals.positive", "Erwarte die selben Objekte. Gegeben:\n%s\nErwartet:\n%s")
		s:set("assertion.equals.negative", "Erwarte nicht die selben Objekte. Gegeben:\n%s\nNicht erwartet:\n%s")

		s:set("assertion.unique.positive", "Erwarte einzigartiges Objekt:\n%s")
		s:set("assertion.unique.negative", "Erwarte nicht einzigartiges Objekt:\n%s")

		s:set("assertion.error.positive", "Es wird ein Fehler erwartet.")
		s:set("assertion.error.negative", "Es wird kein Fehler erwartet, aber folgender Fehler trat auf:\n%s")

		s:set("assertion.truthy.positive", "Erwarte, dass der Wert 'wahr' (truthy) ist. Gegeben:\n%s")
		s:set("assertion.truthy.negative", "Erwarte, dass der Wert 'unwahr' ist (falsy). Gegeben:\n%s")

		s:set("assertion.falsy.positive", "Erwarte, dass der Wert 'unwahr' ist (falsy). Gegeben:\n%s")
		s:set("assertion.falsy.negative", "Erwarte, dass der Wert 'wahr' (truthy) ist. Gegeben:\n%s")

		s:set("assertion.called.positive", "Erwarte, dass die Funktion %s mal aufgerufen wird, anstatt %s mal.")
		s:set("assertion.called.negative", "Erwarte, dass die Funktion nicht genau %s mal aufgerufen wird.")

		s:set("assertion.called_with.positive", "Erwarte, dass die Funktion mit den gegebenen Parametern aufgerufen wird.")
		s:set("assertion.called_with.negative", "Erwarte, dass die Funktion nicht mit den gegebenen Parametern aufgerufen wird.")

		s:set("assertion.returned_arguments.positive", "Aufruf sollte mit Argument %s erfolgen, erfolgte aber mit %s")
		s:set("assertion.returned_arguments.negative", "Aufruf sollte nicht mit Argument %s erfolgen, erfolgte aber mit %s")

		-- errors
		s:set("assertion.internal.argtolittle", "Die Funktion '%s' erwartet mindestens %s Parameter, gegeben: %s")
		s:set("assertion.internal.badargtype", "Die Funktion '%s' erwartet einen Parameter vom Typ '%s', gegeben: %s")
	elseif strLocal == "frFR" then
		s:set_namespace("fr")

		s:set("assertion.same.positive", "Objets supposes de meme nature attendus. Argument passe:\n%s\nAttendu:\n%s")
		s:set("assertion.same.negative", "Objets supposes de natures differentes attendus. Argument passe:\n%s\nNon attendu:\n%s")

		s:set("assertion.equals.positive", "Objets supposes etre de valeur egale attendus. Argument passe:\n%s\nAttendu:\n%s")
		s:set("assertion.equals.negative", "Objets supposes etre de valeurs differentes attendu. Argument passe:\n%s\nNon attendu:\n%s")

		s:set("assertion.unique.positive", "Objet suppose etre unique attendu:\n%s")
		s:set("assertion.unique.negative", "Objet suppose ne pas etre unique attendu:\n%s")
		
		s:set("assertion.error.positive", "Erreur supposee etre generee.")
		s:set("assertion.error.negative", "Erreur non supposee etre generee.\n%s")

		s:set("assertion.truthy.positive", "Assertion supposee etre vraie mais de valeur:\n%s")
		s:set("assertion.truthy.negative", "Assertion supposee etre fausse mais de valeur:\n%s")

		s:set("assertion.falsy.positive", "Assertion supposee etre fausse mais de valeur:\n%s")
		s:set("assertion.falsy.negative", "Assertion supposee etre vraie mais de valeur:\n%s")

		s:set("assertion.called.positive", "%s appel(s) attendu(s) mais %s appel(s) reçu(s) à la place.")
		s:set("assertion.called.negative", "N'aurais pas du être appelé exactement %s fois, mais c'était le cas.")

		s:set("assertion.called_with.positive", "Fonction n'a pas été appelé avec les arguments.")
		s:set("assertion.called_with.negative", "Fonction appelé avec les arguments.")

		s:set("assertion.returned_arguments.positive", "%s argument(s) attendu(s) mais %s argument(s) reçu(s).")
		s:set("assertion.returned_arguments.negative", "N'aurais pas du être appelé avec exactment %s argument(s), mais c'était le cas.")

		-- errors
		s:set("assertion.internal.argtolittle", "Le fonction '%s' nécessite un minimum de %s arguments, reçu: %s")
		s:set("assertion.internal.badargtype", "Le fonction '%s' nécessite un %s comme argument, reçu: %s")
	end
end

Apollo.RegisterPackage(Lib, MAJOR, MINOR, {})
