local MAJOR,MINOR = "Lib:Busted-2.0", 3
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion or 0) >= MINOR then
	return -- no upgrade needed
end
-- Set a reference to the actual package or create an empty table
local busted = APkg and APkg.tPackage or {}

local BustedTests   = setmetatable({}, { __index = function(tbl, key) tbl[key] = {} return tbl[key] end })

-------------------------------------------------------------------------------
--- Olivine-Labs Say
-------------------------------------------------------------------------------

local SAY_MAJOR, SAY_MINOR = "Olivine:Say-1.0", 2
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
  function s:OnLoad() end
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
--- Olivine-Labs spy
-------------------------------------------------------------------------------

local spy
do
  -- Spy metatable
  local spy_mt = {
    __call = function(self, ...)
      local arguments = {...}
      arguments.n = select('#',...)  -- add argument count for trailing nils
      table.insert(self.calls, arguments)
      return self.callback(...)
    end }

  spy = {
    new = function(callback)
      if not util.callable(callback) then
        error("Cannot spy on type '" .. type(callback) .. "', only on functions or callable elements", 2)
      end
      local s = setmetatable(
      {
        calls = {},
        callback = callback,

        target_table = nil, -- these will be set when using 'spy.on'
        target_key = nil,

        revert = function(self)
          if not self.reverted then
            if self.target_table and self.target_key then
              self.target_table[self.target_key] = self.callback
            end
            self.reverted = true
          end
          return self.callback
        end,

        called = function(self, times)
          if times then
            return (#self.calls == times), #self.calls
          end

          return (#self.calls > 0), #self.calls
        end,

        called_with = function(self, args)
          for _,v in ipairs(self.calls) do
            if util.deepcompare(v, args) then
              return true
            end
          end
          return false
        end
      }, spy_mt)
      assert:add_spy(s)  -- register with the current state
      return s
    end,

    is_spy = function(object)
      return type(object) == "table" and getmetatable(object) == spy_mt
    end,

    on = function(target_table, target_key)
      local s = spy.new(target_table[target_key])
      target_table[target_key] = s
      -- store original data
      s.target_table = target_table
      s.target_key = target_key

      return s
    end
  }

  local function set_spy(state)
  end

  local function called_with(state, arguments)
    if rawget(state, "payload") and rawget(state, "payload").called_with then
      return state.payload:called_with(arguments)
    else
      error("'called_with' must be chained after 'spy(aspy)'")
    end
  end

  local function called(state, arguments)
    local num_times = arguments[1]
    if state.payload and type(state.payload) == "table" and state.payload.called then
      local result, count = state.payload:called(num_times)
      arguments[1] = tostring(arguments[1])
      table.insert(arguments, 2, tostring(count))
      arguments.n = arguments.n + 1
      arguments.nofmt = arguments.nofmt or {}
      arguments.nofmt[1] = true
      arguments.nofmt[2] = true
      return result
    elseif state.payload and type(state.payload) == "function" then
      error("When calling 'spy(aspy)', 'aspy' must not be the original function, but the spy function replacing the original")
    else
      error("'called_with' must be chained after 'spy(aspy)'")
    end
  end

  function spy.init()
    assert:register("modifier", "spy", set_spy)
    assert:register("assertion", "called_with", called_with, "assertion.called_with.positive", "assertion.called_with.negative")
    assert:register("assertion", "called", called, "assertion.called.positive", "assertion.called.negative")
  end
end

-------------------------------------------------------------------------------
--- Olivine-Labs stub
-------------------------------------------------------------------------------

local stub = {}
do
  local stubfunc = function() end

  function stub.new(object, key, func)
    if object == nil and key == nil then
      -- called without arguments, create a 'blank' stub
      object = {}
      key = ""
    end
    assert(type(object) == "table" and key ~= nil, "stub.new(): Can only create stub on a table key, call with 2 params; table, key")
    assert(object[key] == nil or util.callable(object[key]), "stub.new(): The element for which to create a stub must either be callable, or be nil")
    local old_elem = object[key]    -- keep existing element (might be nil!)
		object[key] = type(func) == "function" and func or stubfunc  -- set the stubfunction
	  local s = spy.on(object, key)   -- create a spy on top of the stub function
    local spy_revert = s.revert     -- keep created revert function

    s.revert = function(self)       -- wrap revert function to restore original element
      if not self.reverted then
        spy_revert(self)
        object[key] = old_elem
        self.reverted = true
      end
      return old_elem
    end

    return s
  end

  function stub.is_stub(object)
    return spy.is_spy(object) and object.callback == stubfunc
  end

  local function set_stub(state)
  end

  stub = setmetatable( stub, {
      __call = function(self, ...)
        -- stub originally was a function only. Now that it is a module table
        -- the __call method is required for backward compatibility
        -- NOTE: this deviates from spy, which has no __call method
        return stub.new(...)
      end })

  function stub.init()
    assert:register("modifier", "stub", set_stub)
  end
end

-------------------------------------------------------------------------------
--- Olivine-Labs mock
-------------------------------------------------------------------------------

local function mock(object, dostub, func, self, key)
  local data_type = type(object)
  if data_type == "table" then
    if spy.is_spy(object) then
      -- this table is a function already wrapped as a spy, so nothing to do here
    else
      for k,v in pairs(object) do
        object[k] = mock(v, dostub, func, object, k)
      end
    end
  elseif data_type == "userdata" then
    mock(getmetatable(object), dostub, func, self, key)
  elseif data_type == "function" then
    if dostub then
      return stub(self, key, func)
    elseif self==nil then
      return spy.new(object)
    else
      return spy.on(self, key)
    end
  end
  return object
end

local function unmock(object)
  local mockedObject = type(object) == "userdata" and getmetatable(object) or object
  if type(mockedObject) == "table" then
    if spy.is_spy(mockedObject) then
      mockedObject:revert()
    else
      for k,v in pairs(mockedObject) do
        unmock(v)
      end
    end
  end
end

-------------------------------------------------------------------------------
--- Olivine-Labs Mediator
-------------------------------------------------------------------------------

local mediatorProto
do
  local function Subscriber(fn, options)
    return {
      options = options or {},
      fn = fn,
      channel = nil,
      id = math.random(1000000000), -- sounds reasonable, rite?
      update = function(self, options)
        if options then
          self.fn = options.fn or self.fn
          self.options = options.options or self.options
        end
      end
    }
  end

  -- Channel class and functions --

  local function Channel(namespace, parent)
    return {
      stopped = false,
      namespace = namespace,
      callbacks = {},
      channels = {},
      parent = parent,

      addSubscriber = function(self, fn, options)
        local callback = Subscriber(fn, options)
        local priority = (#self.callbacks + 1)

        options = options or {}

        if options.priority and
          options.priority >= 0 and
          options.priority < priority
        then
            priority = options.priority
        end

        table.insert(self.callbacks, priority, callback)

        return callback
      end,

      getSubscriber = function(self, id)
        for i=1, #self.callbacks do
          local callback = self.callbacks[i]
          if callback.id == id then return { index = i, value = callback } end
        end
        local sub
        for _, channel in pairs(self.channels) do
          sub = channel:getSubscriber(id)
          if sub then break end
        end
        return sub
      end,

      setPriority = function(self, id, priority)
        local callback = self:getSubscriber(id)

        if callback.value then
          table.remove(self.callbacks, callback.index)
          table.insert(self.callbacks, priority, callback.value)
        end
      end,

      addChannel = function(self, namespace)
        self.channels[namespace] = Channel(namespace, self)
        return self.channels[namespace]
      end,

      hasChannel = function(self, namespace)
        return namespace and self.channels[namespace] and true
      end,

      getChannel = function(self, namespace)
        return self.channels[namespace] or self:addChannel(namespace)
      end,

      removeSubscriber = function(self, id)
        local callback = self:getSubscriber(id)

        if callback and callback.value then
          for _, channel in pairs(self.channels) do
            channel:removeSubscriber(id)
          end

          return table.remove(self.callbacks, callback.index)
        end
      end,

      publish = function(self, result, ...)
        for i = 1, #self.callbacks do
          local callback = self.callbacks[i]

          -- if it doesn't have a predicate, or it does and it's true then run it
          if not callback.options.predicate or callback.options.predicate(...) then
             -- just take the first result and insert it into the result table
            local value, continue = callback.fn(...)

            if value then table.insert(result, value) end
            if not continue then return result end
          end
        end

        if parent then
          return parent:publish(result, ...)
        else
          return result
        end
      end
    }
  end

  -- Mediator class and functions --

  mediatorProto = setmetatable(
  {
    Channel = Channel,
    Subscriber = Subscriber
  },
  {
    __call = function (fn, options)
      return {
        channel = Channel('root'),

        getChannel = function(self, channelNamespace)
          local channel = self.channel

          for i=1, #channelNamespace do
            channel = channel:getChannel(channelNamespace[i])
          end

          return channel
        end,

        subscribe = function(self, channelNamespace, fn, options)
          return self:getChannel(channelNamespace):addSubscriber(fn, options)
        end,

        getSubscriber = function(self, id, channelNamespace)
          return self:getChannel(channelNamespace):getSubscriber(id)
        end,

        removeSubscriber = function(self, id, channelNamespace)
          return self:getChannel(channelNamespace):removeSubscriber(id)
        end,

        publish = function(self, channelNamespace, ...)
          return self:getChannel(channelNamespace):publish({}, ...)
        end
      }
    end
  })
end

-------------------------------------------------------------------------------
--- Olivine-Labs Busted-Context
-------------------------------------------------------------------------------

local context = {}
do
  local data = {}
  local parents = {}
  local children = {}

  function context.ref()
    local ref = {}
    local ctx = data

    function ref.get(key)
      if not key then return ctx end
      return ctx[key]
    end

    function ref.set(key, value)
      ctx[key] = value
    end

    function ref.attach(child)
      if not children[ctx] then children[ctx] = {} end
      parents[child] = ctx
      children[ctx][#children[ctx]+1] = child
    end

    function ref.children(parent)
      return children[parent] or {}
    end

    function ref.parent(child)
      return parents[child]
    end

    function ref.push(child)
      if not parents[child] then error('Detached child. Cannot push.') end
      ctx = child
    end

    function ref.pop()
			if parents[ctx] then -- TODO: Determine if this is the correct fix
      	ctx = parents[ctx]
			end
    end

		function ref.reset()
			for k,v in pairs(ctx) do
				if k ~= "env" then
					ctx[k] = nil
				end
			end
			parents = {}
			children = {}
		end

    return ref
  end
end

-------------------------------------------------------------------------------
--- Olivine-Labs Busted-Environment
-------------------------------------------------------------------------------

local function buildEnvironment(context)
  local environment = {}

  local function getEnv(self, key)
    if not self then return nil end
    return
      self.env and self.env[key] or
      getEnv(context.parent(self), key) or
      _G[key]
  end

  local function setEnv(self, key, value)
    if not self.env then self.env = {} end
    self.env[key] = value
  end

  local function __index(self, key)
    return getEnv(context.get(), key)
  end

  local function __newindex(self, key, value)
    setEnv(context.get(), key, value)
  end

  local env = setmetatable({}, { __index=__index, __newindex=__newindex })

  function environment.wrap(fn)
    return setfenv(fn, env)
  end

  function environment.set(key, value)
    local env = context.get('env')

    if not env then
      env = {}
      context.set('env', env)
    end

    env[key] = value
  end
  return environment
end


-------------------------------------------------------------------------------
--- Olivine-Labs Busted-Core
-------------------------------------------------------------------------------

do
  local mediator = mediatorProto()
  busted.version = '2.0-1'

  busted.context = context.ref()

  local environment = buildEnvironment(busted.context)
  environment.set('mock', mock)
  environment.set('unmock', unmock)
  environment.set('spy', spy)
  environment.set('stub', stub)
  environment.set('s', s)

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
end

-------------------------------------------------------------------------------
--- Olivine-Labs Busted-Done
-------------------------------------------------------------------------------

local BustedDone
do
  local M = {}

  -- adds tokens to the current wait list, does not change order/unordered
  M.wait = function(self, ...)
    local tlist = { ... }

    for _, token in ipairs(tlist) do
      if type(token) ~= "string" then
        error("Wait tokens must be strings. Got "..type(token), 2)
      end
      table.insert(self.tokens, token)
    end
  end

  -- set list as unordered, adds tokens to current wait list
  M.wait_unordered = function(self, ...)
    self.ordered = false
    self:wait(...)
  end

  -- set list as ordered, adds tokens to current wait list
  M.wait_ordered = function(self, ...)
    self.ordered = true
    self:wait(...)
  end

  -- generates a message listing tokens received/open
  M.tokenlist = function(self)
    local list

    if #self.tokens_done == 0 then
      list = "No tokens received."
    else
      list = "Tokens received ("..tostring(#self.tokens_done)..")"
      local s = ": "

      for _,t in ipairs(self.tokens_done) do
        list = list .. s .. "'"..t.."'"
        s = ", "
      end

      list = list .. "."
    end

    if #self.tokens == 0 then
      list = list .. " No more tokens expected."
    else
      list = list .. " Tokens not received ("..tostring(#self.tokens)..")"
      local s = ": "

      for _, t in ipairs(self.tokens) do
        list = list .. s .. "'"..t.."'"
        s = ", "
      end

      list = list .. "."
    end

    return list
  end

  -- marks a token as completed, checks for ordered/unordered, checks for completeness
  M.done = function(self, ...) self:_done(...) end  -- extra wrapper for same error level constant as __call method
  M._done = function(self, token)
    if token then
      if type(token) ~= "string" then
        error("Wait tokens must be strings. Got "..type(token), 3)
      end

      if self.ordered then
        if self.tokens[1] == token then
          table.remove(self.tokens, 1)
          table.insert(self.tokens_done, token)
        else
          if self.tokens[1] then
            error(("Bad token, expected '%s' got '%s'. %s"):format(self.tokens[1], token, self:tokenlist()), 3)
          else
            error(("Bad token (no more tokens expected) got '%s'. %s"):format(token, self:tokenlist()), 3)
          end
        end
      else
        -- unordered
        for i, t in ipairs(self.tokens) do
          if t == token then
            table.remove(self.tokens, i)
            table.insert(self.tokens_done, token)
            token = nil
            break
          end
        end

        if token then
          error(("Unknown token '%s'. %s"):format(token, self:tokenlist()), 3)
        end
      end
    end
    if not next(self.tokens) then
      -- no more tokens, so we're really done...
      self.done_cb()
    end
  end


  -- wraps a done callback into a done-object supporting tokens to sign-off
  M.new = function(done_callback)
    local obj = {
      tokens = {},
      tokens_done = {},
      done_cb = done_callback,
      ordered = true,  -- default for sign off of tokens
    }

    return setmetatable( obj, {
      __call = function(self, ...)
        self:_done(...)
      end,
      __index = M,
    })
  end

  BustedDone = M
end

-------------------------------------------------------------------------------
--- Olivine-Labs Busted-Init
-------------------------------------------------------------------------------

--math.randomseed(os.time())
local bustedInit
do
  local function shuffle(t)
    local n = #t
    while n >= 2 do
      local k = math.random(n)
      t[n], t[k] = t[k], t[n]
      n = n - 1
    end
    return t
  end

  bustedInit = function(busted)
    local function execAll(descriptor, current, propagate)
      local parent = busted.context.parent(current)

      if propagate and parent then execAll(descriptor, parent, propagate) end

      local list = current[descriptor]

      if list then
        for _, v in pairs(list) do
          busted.safe(descriptor, v.run, v)
        end
      end
    end

    local function dexecAll(descriptor, current, propagate)
      local parent = busted.context.parent(current)
      local list = current[descriptor]

      if list then
        for _, v in pairs(list) do
          busted.safe(descriptor, v.run, v)
        end
      end

      if propagate and parent then execAll(descriptor, parent, propagate) end
    end

    local file = function(file)
      busted.publish({ 'file', 'start' }, file.name)

      if busted.safe('file', file.run, file, true) then
        busted.execute(file)
      end

      busted.publish({ 'file', 'end' }, file.name)
    end

    local describe = function(describe)
      local parent = busted.context.parent(describe)

      busted.publish({ 'describe', 'start' }, describe, parent)

      if not describe.env then describe.env = {} end

      local randomize = false
      describe.env.randomize = function()
        randomize = true
      end

      if busted.safe('describe', describe.run, describe) then
        if randomize then
          shuffle(busted.context.children(describe))
        end
        execAll('setup', describe)
        busted.execute(describe)
        dexecAll('teardown', describe)
      end

      busted.publish({ 'describe', 'end' }, describe, parent)
    end

    local it = function(it)
      local finally

      if not it.env then it.env = {} end

      it.env.finally = function(fn)
        finally = fn
      end

      local parent = busted.context.parent(it)

      execAll('before_each', parent, true)
      busted.publish({ 'test', 'start' }, it, parent)
      local res = busted.safe('it', it.run, it)
      if not it.env.done then
        busted.publish({ 'test', 'end' }, it, parent, res and 'success' or 'failure')
        if finally then busted.safe('finally', finally, it) end
        dexecAll('after_each', parent, true)
      end
    end

    local pending = function(pending)
      local trace = busted.getTrace(pending, 3)
      busted.publish({ 'test', 'end' }, pending, busted.context.parent(pending), 'pending', trace)
    end

    local async = function()
      local parent = busted.context.get()
      if not parent.env then parent.env = {} end

      parent.env.done = BustedDone.new(function()
        busted.publish({ 'test', 'end' }, it, parent, 'success')
        if finally then busted.safe('finally', finally, it) end
        dexecAll('after_each', parent, true)
      end)
    end

    busted.register('file', file)

    busted.register('describe', describe)
    busted.register('context', describe)

    busted.register('it', it)
    busted.register('pending', pending)

    busted.context.get().env.async = async

    busted.register('setup')
    busted.register('teardown')
    busted.register('before_each')
    busted.register('after_each')

    return busted
  end
end

-------------------------------------------------------------------------------
--- Penlight (https://github.com/stevedonovan/Penlight/) - Operator
-------------------------------------------------------------------------------

local operator = {}
do
  --- Lua operators available as functions.
  --
  -- (similar to the Python module of the same name)
  --
  -- There is a module field `optable` which maps the operator strings
  -- onto these functions, e.g. `operator.optable['()']==operator.call`
  --
  -- Operator strings like '>' and '{}' can be passed to most Penlight functions
  -- expecting a function argument.
  --
  -- Dependencies: `pl.utils`
  -- @module pl.operator

  local strfind = string.find

  --- apply function to some arguments **()**
  -- @param fn a function or callable object
  -- @param ... arguments
  function operator.call(fn,...)
      return fn(...)
  end

  --- get the indexed value from a table **[]**
  -- @param t a table or any indexable object
  -- @param k the key
  function  operator.index(t,k)
      return t[k]
  end

  --- returns true if arguments are equal **==**
  -- @param a value
  -- @param b value
  function  operator.eq(a,b)
      return a==b
  end

  --- returns true if arguments are not equal **~=**
   -- @param a value
  -- @param b value
  function  operator.neq(a,b)
      return a~=b
  end

  --- returns true if a is less than b **<**
  -- @param a value
  -- @param b value
  function  operator.lt(a,b)
      return a < b
  end

  --- returns true if a is less or equal to b **<=**
  -- @param a value
  -- @param b value
  function  operator.le(a,b)
      return a <= b
  end

  --- returns true if a is greater than b **>**
  -- @param a value
  -- @param b value
  function  operator.gt(a,b)
      return a > b
  end

  --- returns true if a is greater or equal to b **>=**
  -- @param a value
  -- @param b value
  function  operator.ge(a,b)
      return a >= b
  end

  --- returns length of string or table **#**
  -- @param a a string or a table
  function  operator.len(a)
      return #a
  end

  --- add two values **+**
  -- @param a value
  -- @param b value
  function  operator.add(a,b)
      return a+b
  end

  --- subtract b from a **-**
  -- @param a value
  -- @param b value
  function  operator.sub(a,b)
      return a-b
  end

  --- multiply two values __*__
  -- @param a value
  -- @param b value
  function  operator.mul(a,b)
      return a*b
  end

  --- divide first value by second **/**
  -- @param a value
  -- @param b value
  function  operator.div(a,b)
      return a/b
  end

  --- raise first to the power of second **^**
  -- @param a value
  -- @param b value
  function  operator.pow(a,b)
      return a^b
  end

  --- modulo; remainder of a divided by b **%**
  -- @param a value
  -- @param b value
  function  operator.mod(a,b)
      return a%b
  end

  --- concatenate two values (either strings or `__concat` defined) **..**
  -- @param a value
  -- @param b value
  function  operator.concat(a,b)
      return a..b
  end

  --- return the negative of a value **-**
  -- @param a value
  function  operator.unm(a)
      return -a
  end

  --- false if value evaluates as true **not**
  -- @param a value
  function  operator.lnot(a)
      return not a
  end

  --- true if both values evaluate as true **and**
  -- @param a value
  -- @param b value
  function  operator.land(a,b)
      return a and b
  end

  --- true if either value evaluate as true **or**
  -- @param a value
  -- @param b value
  function  operator.lor(a,b)
      return a or b
  end

  --- make a table from the arguments **{}**
  -- @param ... non-nil arguments
  -- @return a table
  function  operator.table (...)
      return {...}
  end

  --- match two strings **~**.
  -- uses @{string.find}
  function  operator.match (a,b)
      return strfind(a,b)~=nil
  end

  --- the null operation.
  -- @param ... arguments
  -- @return the arguments
  function  operator.nop (...)
      return ...
  end

  ---- Map from operator symbol to function.
  -- Most of these map directly from operators;
  -- But note these extras
  --
  --  * __'()'__  `call`
  --  * __'[]'__  `index`
  --  * __'{}'__ `table`
  --  * __'~'__   `match`
  --
  -- @table optable
  -- @field operator
   operator.optable = {
      ['+']=operator.add,
      ['-']=operator.sub,
      ['*']=operator.mul,
      ['/']=operator.div,
      ['%']=operator.mod,
      ['^']=operator.pow,
      ['..']=operator.concat,
      ['()']=operator.call,
      ['[]']=operator.index,
      ['<']=operator.lt,
      ['<=']=operator.le,
      ['>']=operator.gt,
      ['>=']=operator.ge,
      ['==']=operator.eq,
      ['~=']=operator.neq,
      ['#']=operator.len,
      ['and']=operator.land,
      ['or']=operator.lor,
      ['{}']=operator.table,
      ['~']=operator.match,
      ['']=operator.nop,
  }
end

-------------------------------------------------------------------------------
--- Penlight (https://github.com/stevedonovan/Penlight/) - Utils
-------------------------------------------------------------------------------

local utils = {
  _VERSION = "1.2.1",
  lua51 = true,
  setfenv = setfenv,
  getfenv = getfenv,
  load = loadstring,
  execute = function() error("No os.execute Available!") end,
  dir_separator = _G.package.config:sub(1,1),
  unpack = unpack,
}
do
  --- Generally useful routines.
  -- See  @{01-introduction.md.Generally_useful_functions|the Guide}.
  -- @module pl.utils
  local format,gsub,byte = string.format,string.gsub,string.byte
  local clock = os.clock
  local append = table.insert
  local unpack = rawget(_G,'unpack') or rawget(table,'unpack')

  local collisions = {}

  --- end this program gracefully.
  -- @param code The exit code or a message to be printed
  -- @param ... extra arguments for message's format'
  -- @see utils.fprintf
  function utils.quit(code,...)
      if type(code) == 'string' then
          error("Quit unsupported: " .. string.format(code, ...))
          utils.fprintf(io.stderr,code,...)
          code = -1
      else
          error("Quit unsupported: " .. string.format(...))
      end
  end

  utils.patterns = {
      FLOAT = '[%+%-%d]%d*%.?%d*[eE]?[%+%-]?%d*',
      INTEGER = '[+%-%d]%d*',
      IDEN = '[%a_][%w_]*',
      FILE = '[%a%.\\][:%][%w%._%-\\]*'
  }

  --- escape any 'magic' characters in a string
  -- @param s The input string
  function utils.escape(s)
      utils.assert_string(1,s)
      return (s:gsub('[%-%.%+%[%]%(%)%$%^%%%?%*]','%%%1'))
  end

  local raise

  --- split a string into a list of strings separated by a delimiter.
  -- @param s The input string
  -- @param re A Lua string pattern; defaults to '%s+'
  -- @param plain don't use Lua patterns
  -- @param n optional maximum number of splits
  -- @return a list-like table
  -- @raise error if s is not a string
  function utils.split(s,re,plain,n)
      utils.assert_string(1,s)
      local find,sub,append = string.find, string.sub, table.insert
      local i1,ls = 1,{}
      if not re then re = '%s+' end
      if re == '' then return {s} end
      while true do
          local i2,i3 = find(s,re,i1,plain)
          if not i2 then
              local last = sub(s,i1)
              if last ~= '' then append(ls,last) end
              if #ls == 1 and ls[1] == '' then
                  return {}
              else
                  return ls
              end
          end
          append(ls,sub(s,i1,i2-1))
          if n and #ls == n then
              ls[#ls] = sub(s,i1)
              return ls
          end
          i1 = i3+1
      end
  end

  --- split a string into a number of values.
  -- @param s the string
  -- @param re the delimiter, default space
  -- @return n values
  -- @usage first,next = splitv('jane:doe',':')
  -- @see split
  function utils.splitv (s,re)
      return unpack(utils.split(s,re))
  end

  --- convert an array of values to strings.
  -- @param t a list-like table
  -- @param temp buffer to use, otherwise allocate
  -- @param tostr custom tostring function, called with (value,index).
  -- Otherwise use `tostring`
  -- @return the converted buffer
  function utils.array_tostring (t,temp,tostr)
      temp, tostr = temp or {}, tostr or tostring
      for i = 1,#t do
          temp[i] = tostr(t[i],i)
      end
      return temp
  end

  --- 'memoize' a function (cache returned value for next call).
  -- This is useful if you have a function which is relatively expensive,
  -- but you don't know in advance what values will be required, so
  -- building a table upfront is wasteful/impossible.
  -- @param func a function of at least one argument
  -- @return a function with at least one argument, which is used as the key.
  function utils.memoize(func)
      return setmetatable({}, {
          __index = function(self, k, ...)
              local v = func(k,...)
              self[k] = v
              return v
          end,
          __call = function(self, k) return self[k] end
      })
  end


  utils.stdmt = {
      List = {_name='List'}, Map = {_name='Map'},
      Set = {_name='Set'}, MultiMap = {_name='MultiMap'}
  }

  local _function_factories = {}

  --- associate a function factory with a type.
  -- A function factory takes an object of the given type and
  -- returns a function for evaluating it
  -- @tab mt metatable
  -- @func fun a callable that returns a function
  function utils.add_function_factory (mt,fun)
      _function_factories[mt] = fun
  end

  local function _string_lambda(f)
      local raise = utils.raise
      if f:find '^|' or f:find '_' then
          local args,body = f:match '|([^|]*)|(.+)'
          if f:find '_' then
              args = '_'
              body = f
          else
              if not args then return raise 'bad string lambda' end
          end
          local fstr = 'return function('..args..') return '..body..' end'
          local fn,err = utils.load(fstr)
          if not fn then return raise(err) end
          fn = fn()
          return fn
      else return raise 'not a string lambda'
      end
  end

  --- an anonymous function as a string. This string is either of the form
  -- '|args| expression' or is a function of one argument, '_'
  -- @param lf function as a string
  -- @return a function
  -- @usage string_lambda '|x|x+1' (2) == 3
  -- @usage string_lambda '_+1 (2) == 3
  -- @function utils.string_lambda
  utils.string_lambda = utils.memoize(_string_lambda)

  local ops

  --- process a function argument.
  -- This is used throughout Penlight and defines what is meant by a function:
  -- Something that is callable, or an operator string as defined by <code>pl.operator</code>,
  -- such as '>' or '#'. If a function factory has been registered for the type, it will
  -- be called to get the function.
  -- @param idx argument index
  -- @param f a function, operator string, or callable object
  -- @param msg optional error message
  -- @return a callable
  -- @raise if idx is not a number or if f is not callable
  function utils.function_arg (idx,f,msg)
      utils.assert_arg(1,idx,'number')
      local tp = type(f)
      if tp == 'function' then return f end  -- no worries!
      -- ok, a string can correspond to an operator (like '==')
      if tp == 'string' then
          if not ops then ops = operator.optable end
          local fn = ops[f]
          if fn then return fn end
          local fn, err = utils.string_lambda(f)
          if not fn then error(err..': '..f) end
          return fn
      elseif tp == 'table' or tp == 'userdata' then
          local mt = getmetatable(f)
          if not mt then error('not a callable object',2) end
          local ff = _function_factories[mt]
          if not ff then
              if not mt.__call then error('not a callable object',2) end
              return f
          else
              return ff(f) -- we have a function factory for this type!
          end
      end
      if not msg then msg = " must be callable" end
      if idx > 0 then
          error("argument "..idx..": "..msg,2)
      else
          error(msg,2)
      end
  end

  --- assert that the given argument is in fact of the correct type.
  -- @param n argument index
  -- @param val the value
  -- @param tp the type
  -- @param verify an optional verfication function
  -- @param msg an optional custom message
  -- @param lev optional stack position for trace, default 2
  -- @raise if the argument n is not the correct type
  -- @usage assert_arg(1,t,'table')
  -- @usage assert_arg(n,val,'string',path.isdir,'not a directory')
  function utils.assert_arg (n,val,tp,verify,msg,lev)
      if type(val) ~= tp then
          error(("argument %d expected a '%s', got a '%s'"):format(n,tp,type(val)),lev or 2)
      end
      if verify and not verify(val) then
          error(("argument %d: '%s' %s"):format(n,val,msg),lev or 2)
      end
  end

  --- assert the common case that the argument is a string.
  -- @param n argument index
  -- @param val a value that must be a string
  -- @raise val must be a string
  function utils.assert_string (n,val)
      utils.assert_arg(n,val,'string',nil,nil,3)
  end

  local err_mode = 'default'

  --- control the error strategy used by Penlight.
  -- Controls how <code>utils.raise</code> works; the default is for it
  -- to return nil and the error string, but if the mode is 'error' then
  -- it will throw an error. If mode is 'quit' it will immediately terminate
  -- the program.
  -- @param mode - either 'default', 'quit'  or 'error'
  -- @see utils.raise
  function utils.on_error (mode)
      if ({['default'] = 1, ['quit'] = 2, ['error'] = 3})[mode] then
        err_mode = mode
      else
        -- fail loudly
        if err_mode == 'default' then err_mode = 'error' end
        utils.raise("Bad argument expected string; 'default', 'quit', or 'error'. Got '"..tostring(mode).."'")
      end
  end

  --- used by Penlight functions to return errors.  Its global behaviour is controlled
  -- by <code>utils.on_error</code>
  -- @param err the error string.
  -- @see utils.on_error
  function utils.raise (err)
      if err_mode == 'default' then return nil,err
      elseif err_mode == 'quit' then utils.quit(err)
      else error(err,2)
      end
  end

  --- is the object of the specified type?.
  -- If the type is a string, then use type, otherwise compare with metatable
  -- @param obj An object to check
  -- @param tp String of what type it should be
  function utils.is_type (obj,tp)
      if type(tp) == 'string' then return type(obj) == tp end
      local mt = getmetatable(obj)
      return tp == mt
  end

  raise = utils.raise

  --- load a code string or bytecode chunk.
  -- @param code Lua code as a string or bytecode
  -- @param name for source errors
  -- @param mode kind of chunk, 't' for text, 'b' for bytecode, 'bt' for all (default)
  -- @param env  the environment for the new chunk (default nil)
  -- @return compiled chunk
  -- @return error message (chunk is nil)
  -- @function utils.load

  ---------------
  -- Get environment of a function.
  -- With Lua 5.2, may return nil for a function with no global references!
  -- Based on code by [Sergey Rozhenko](http://lua-users.org/lists/lua-l/2010-06/msg00313.html)
  -- @param f a function or a call stack reference
  -- @function utils.setfenv

  ---------------
  -- Set environment of a function
  -- @param f a function or a call stack reference
  -- @param env a table that becomes the new environment of `f`
  -- @function utils.setfenv

  --- execute a shell command.
  -- This is a compatibility function that returns the same for Lua 5.1 and Lua 5.2
  -- @param cmd a shell command
  -- @return true if successful
  -- @return actual return code
  -- @function utils.execute
end

-------------------------------------------------------------------------------
--- Penlight (https://github.com/stevedonovan/Penlight/) - Types
-------------------------------------------------------------------------------
local types = {}
do
  ---- Dealing with Detailed Type Information

  -- Dependencies `pl.utils`
  -- @module pl.types


  --- is the object either a function or a callable object?.
  -- @param obj Object to check.
  function types.is_callable (obj)
      return type(obj) == 'function' or getmetatable(obj) and getmetatable(obj).__call
  end

  --- is the object of the specified type?.
  -- If the type is a string, then use type, otherwise compare with metatable
  -- @param obj An object to check
  -- @param tp String of what type it should be
  -- @function is_type
  types.is_type = utils.is_type

  --- a string representation of a type.
  -- For tables with metatables, we assume that the metatable has a `_name`
  -- field. Knows about Lua file objects.
  -- @param obj an object
  -- @return a string like 'number', 'table' or 'List'
  function types.type (obj)
      local t = type(obj)
      if t == 'table' or t == 'userdata' then
          local mt = getmetatable(obj)
          return mt._name or "unknown "..t
      else
          return t
      end
  end

  --- is this number an integer?
  -- @param x a number
  -- @raise error if x is not a number
  function types.is_integer (x)
      return math.ceil(x)==x
  end

  --- Check if the object is "empty".
  -- An object is considered empty if it is nil, a table with out any items (key,
  -- value pairs or indexes), or a string with no content ("").
  -- @param o The object to check if it is empty.
  -- @param ignore_spaces If the object is a string and this is true the string is
  -- considered empty is it only contains spaces.
  -- @return true if the object is empty, otherwise false.
  function types.is_empty(o, ignore_spaces)
      if o == nil or (type(o) == "table" and not next(o)) or (type(o) == "string" and (o == "" or (ignore_spaces and o:match("^%s+$")))) then
          return true
      end
      return false
  end

  local function check_meta (val)
      if type(val) == 'table' then return true end
      return getmetatable(val)
  end

  --- is an object 'array-like'?
  -- @param val any value.
  function types.is_indexable (val)
      local mt = check_meta(val)
      if mt == true then return true end
      return not(mt and mt.__len and mt.__index)
  end

  --- can an object be iterated over with `ipairs`?
  -- @param val any value.
  function types.is_iterable (val)
      local mt = check_meta(val)
      if mt == true then return true end
      return not(mt and mt.__pairs)
  end

  --- can an object accept new key/pair values?
  -- @param val any value.
  function types.is_writeable (val)
      local mt = check_meta(val)
      if mt == true then return true end
      return not(mt and mt.__newindex)
  end

  -- Strings that should evaluate to true.
  local trues = { yes=true, y=true, ["true"]=true, t=true, ["1"]=true }
  -- Conditions types should evaluate to true.
  local true_types = {
      boolean=function(o, true_strs, check_objs) return o end,
      string=function(o, true_strs, check_objs)
          if trues[o:lower()] then
              return true
          end
          -- Check alternative user provided strings.
          for _,v in ipairs(true_strs or {}) do
              if type(v) == "string" and o == v:lower() then
                  return true
              end
          end
          return false
      end,
      number=function(o, true_strs, check_objs) return o ~= 0 end,
      table=function(o, true_strs, check_objs) if check_objs and next(o) ~= nil then return true end return false end
  }
  --- Convert to a boolean value.
  -- True values are:
  --
  -- * boolean: true.
  -- * string: 'yes', 'y', 'true', 't', '1' or additional strings specified by `true_strs`.
  -- * number: Any non-zero value.
  -- * table: Is not empty and `check_objs` is true.
  -- * object: Is not `nil` and `check_objs` is true.
  --
  -- @param o The object to evaluate.
  -- @param[opt] true_strs optional Additional strings that when matched should evaluate to true. Comparison is case insensitive.
  -- This should be a List of strings. E.g. "ja" to support German.
  -- @param[opt] check_objs True if objects should be evaluated. Default is to evaluate objects as true if not nil
  -- or if it is a table and it is not empty.
  -- @return true if the input evaluates to true, otherwise false.
  function types.to_bool(o, true_strs, check_objs)
      local true_func
      if true_strs then
          utils.assert_arg(2, true_strs, "table")
      end
      true_func = true_types[type(o)]
      if true_func then
          return true_func(o, true_strs, check_objs)
      elseif check_objs and o ~= nil then
          return true
      end
      return false
  end
end

-------------------------------------------------------------------------------
--- Penlight (https://github.com/stevedonovan/Penlight/) - Tablex
-------------------------------------------------------------------------------

local tablex = {}
do
  --- Extended operations on Lua tables.
  --
  -- See @{02-arrays.md.Useful_Operations_on_Tables|the Guide}
  --
  -- Dependencies: `pl.utils`, `pl.types`
  -- @module pl.tablex
  local getmetatable,setmetatable = getmetatable,setmetatable
  local tsort,append,remove = table.sort,table.insert,table.remove
  local min,max = math.min,math.max
  local pairs,type,unpack,next,select,tostring = pairs,type,unpack,next,select,tostring
  local function_arg = utils.function_arg
  local Set = utils.stdmt.Set
  local List = utils.stdmt.List
  local Map = utils.stdmt.Map
  local assert_arg = utils.assert_arg

  -- generally, functions that make copies of tables try to preserve the metatable.
  -- However, when the source has no obvious type, then we attach appropriate metatables
  -- like List, Map, etc to the result.
  local function setmeta (res,tbl,def)
      local mt = getmetatable(tbl) or def
      return setmetatable(res, mt)
  end

  local function makelist (res)
      return setmetatable(res,List)
  end

  local function complain (idx,msg)
      error(('argument %d is not %s'):format(idx,msg),3)
  end

  local function assert_arg_indexable (idx,val)
      if not types.is_indexable(val) then
          complain(idx,"indexable")
      end
  end

  local function assert_arg_iterable (idx,val)
      if not types.is_iterable(val) then
          complain(idx,"iterable")
      end
  end

  local function assert_arg_writeable (idx,val)
      if not types.is_writeable(val) then
          complain(idx,"writeable")
      end
  end

  --- copy a table into another, in-place.
  -- @within Copying
  -- @tab t1 destination table
  -- @tab t2 source (actually any iterable object)
  -- @return first table
  function tablex.update (t1,t2)
      assert_arg_writeable(1,t1)
      assert_arg_iterable(2,t2)
      for k,v in pairs(t2) do
          t1[k] = v
      end
      return t1
  end

  --- total number of elements in this table.
  -- Note that this is distinct from `#t`, which is the number
  -- of values in the array part; this value will always
  -- be greater or equal. The difference gives the size of
  -- the hash part, for practical purposes. Works for any
  -- object with a __pairs metamethod.
  -- @tab t a table
  -- @return the size
  function tablex.size (t)
      assert_arg_iterable(1,t)
      local i = 0
      for k in pairs(t) do i = i + 1 end
      return i
  end

  --- make a shallow copy of a table
  -- @within Copying
  -- @tab t an iterable source
  -- @return new table
  function tablex.copy (t)
      assert_arg_iterable(1,t)
      local res = {}
      for k,v in pairs(t) do
          res[k] = v
      end
      return res
  end

  --- make a deep copy of a table, recursively copying all the keys and fields.
  -- This will also set the copied table's metatable to that of the original.
  -- @within Copying
  -- @tab t A table
  -- @return new table
  function tablex.deepcopy(t)
      if type(t) ~= 'table' then return t end
      assert_arg_iterable(1,t)
      local mt = getmetatable(t)
      local res = {}
      for k,v in pairs(t) do
          if type(v) == 'table' then
              v = tablex.deepcopy(v)
          end
          res[k] = v
      end
      setmetatable(res,mt)
      return res
  end

  local abs, deepcompare = math.abs

  --- compare two values.
  -- if they are tables, then compare their keys and fields recursively.
  -- @within Comparing
  -- @param t1 A value
  -- @param t2 A value
  -- @bool[opt] ignore_mt if true, ignore __eq metamethod (default false)
  -- @number[opt] eps if defined, then used for any number comparisons
  -- @return true or false
  function tablex.deepcompare(t1,t2,ignore_mt,eps)
      local ty1 = type(t1)
      local ty2 = type(t2)
      if ty1 ~= ty2 then return false end
      -- non-table types can be directly compared
      if ty1 ~= 'table' then
          if ty1 == 'number' and eps then return abs(t1-t2) < eps end
          return t1 == t2
      end
      -- as well as tables which have the metamethod __eq
      local mt = getmetatable(t1)
      if not ignore_mt and mt and mt.__eq then return t1 == t2 end
      for k1 in pairs(t1) do
          if t2[k1]==nil then return false end
      end
      for k2 in pairs(t2) do
          if t1[k2]==nil then return false end
      end
      for k1,v1 in pairs(t1) do
          local v2 = t2[k1]
          if not deepcompare(v1,v2,ignore_mt,eps) then return false end
      end

      return true
  end

  deepcompare = tablex.deepcompare

  --- compare two arrays using a predicate.
  -- @within Comparing
  -- @array t1 an array
  -- @array t2 an array
  -- @func cmp A comparison function
  function tablex.compare (t1,t2,cmp)
      assert_arg_indexable(1,t1)
      assert_arg_indexable(2,t2)
      if #t1 ~= #t2 then return false end
      cmp = function_arg(3,cmp)
      for k = 1,#t1 do
          if not cmp(t1[k],t2[k]) then return false end
      end
      return true
  end

  --- compare two list-like tables using an optional predicate, without regard for element order.
  -- @within Comparing
  -- @array t1 a list-like table
  -- @array t2 a list-like table
  -- @param cmp A comparison function (may be nil)
  function tablex.compare_no_order (t1,t2,cmp)
      assert_arg_indexable(1,t1)
      assert_arg_indexable(2,t2)
      if cmp then cmp = function_arg(3,cmp) end
      if #t1 ~= #t2 then return false end
      local visited = {}
      for i = 1,#t1 do
          local val = t1[i]
          local gotcha
          for j = 1,#t2 do if not visited[j] then
              local match
              if cmp then match = cmp(val,t2[j]) else match = val == t2[j] end
              if match then
                  gotcha = j
                  break
              end
          end end
          if not gotcha then return false end
          visited[gotcha] = true
      end
      return true
  end


  --- return the index of a value in a list.
  -- Like string.find, there is an optional index to start searching,
  -- which can be negative.
  -- @within Finding
  -- @array t A list-like table
  -- @param val A value
  -- @int idx index to start; -1 means last element,etc (default 1)
  -- @return index of value or nil if not found
  -- @usage find({10,20,30},20) == 2
  -- @usage find({'a','b','a','c'},'a',2) == 3
  function tablex.find(t,val,idx)
      assert_arg_indexable(1,t)
      idx = idx or 1
      if idx < 0 then idx = #t + idx + 1 end
      for i = idx,#t do
          if t[i] == val then return i end
      end
      return nil
  end

  --- return the index of a value in a list, searching from the end.
  -- Like string.find, there is an optional index to start searching,
  -- which can be negative.
  -- @within Finding
  -- @array t A list-like table
  -- @param val A value
  -- @param idx index to start; -1 means last element,etc (default 1)
  -- @return index of value or nil if not found
  -- @usage rfind({10,10,10},10) == 3
  function tablex.rfind(t,val,idx)
      assert_arg_indexable(1,t)
      idx = idx or #t
      if idx < 0 then idx = #t + idx + 1 end
      for i = idx,1,-1 do
          if t[i] == val then return i end
      end
      return nil
  end


  --- return the index (or key) of a value in a table using a comparison function.
  -- @within Finding
  -- @tab t A table
  -- @func cmp A comparison function
  -- @param arg an optional second argument to the function
  -- @return index of value, or nil if not found
  -- @return value returned by comparison function
  function tablex.find_if(t,cmp,arg)
      assert_arg_iterable(1,t)
      cmp = function_arg(2,cmp)
      for k,v in pairs(t) do
          local c = cmp(v,arg)
          if c then return k,c end
      end
      return nil
  end

  --- return a list of all values in a table indexed by another list.
  -- @tab tbl a table
  -- @array idx an index table (a list of keys)
  -- @return a list-like table
  -- @usage index_by({10,20,30,40},{2,4}) == {20,40}
  -- @usage index_by({one=1,two=2,three=3},{'one','three'}) == {1,3}
  function tablex.index_by(tbl,idx)
      assert_arg_indexable(1,tbl)
      assert_arg_indexable(2,idx)
      local res = {}
      for i = 1,#idx do
          res[i] = tbl[idx[i]]
      end
      return setmeta(res,tbl,List)
  end

  --- apply a function to all values of a table.
  -- This returns a table of the results.
  -- Any extra arguments are passed to the function.
  -- @within MappingAndFiltering
  -- @func fun A function that takes at least one argument
  -- @tab t A table
  -- @param ... optional arguments
  -- @usage map(function(v) return v*v end, {10,20,30,fred=2}) is {100,400,900,fred=4}
  function tablex.map(fun,t,...)
      assert_arg_iterable(1,t)
      fun = function_arg(1,fun)
      local res = {}
      for k,v in pairs(t) do
          res[k] = fun(v,...)
      end
      return setmeta(res,t)
  end

  --- apply a function to all values of a list.
  -- This returns a table of the results.
  -- Any extra arguments are passed to the function.
  -- @within MappingAndFiltering
  -- @func fun A function that takes at least one argument
  -- @array t a table (applies to array part)
  -- @param ... optional arguments
  -- @return a list-like table
  -- @usage imap(function(v) return v*v end, {10,20,30,fred=2}) is {100,400,900}
  function tablex.imap(fun,t,...)
      assert_arg_indexable(1,t)
      fun = function_arg(1,fun)
      local res = {}
      for i = 1,#t do
          res[i] = fun(t[i],...) or false
      end
      return setmeta(res,t,List)
  end

  --- apply a named method to values from a table.
  -- @within MappingAndFiltering
  -- @string name the method name
  -- @array t a list-like table
  -- @param ... any extra arguments to the method
  function tablex.map_named_method (name,t,...)
      utils.assert_string(1,name)
      assert_arg_indexable(2,t)
      local res = {}
      for i = 1,#t do
          local val = t[i]
          local fun = val[name]
          res[i] = fun(val,...)
      end
      return setmeta(res,t,List)
  end

  --- apply a function to all values of a table, in-place.
  -- Any extra arguments are passed to the function.
  -- @func fun A function that takes at least one argument
  -- @tab t a table
  -- @param ... extra arguments
  function tablex.transform (fun,t,...)
      assert_arg_iterable(1,t)
      fun = function_arg(1,fun)
      for k,v in pairs(t) do
          t[k] = fun(v,...)
      end
  end

  --- generate a table of all numbers in a range
  -- @int start  number
  -- @int finish number
  -- @int[opt=1] step  (-1 for decreasing)
  function tablex.range (start,finish,step)
      if start == finish then return {start}
      elseif start > finish then return {}
      end
      local res = {}
      local k = 1
      if not step then
          if finish > start then step = finish > start and 1 or -1 end
      end
      for i=start,finish,step do res[k]=i; k=k+1 end
      return res
  end

  --- apply a function to values from two tables.
  -- @within MappingAndFiltering
  -- @func fun a function of at least two arguments
  -- @tab t1 a table
  -- @tab t2 a table
  -- @param ... extra arguments
  -- @return a table
  -- @usage map2('+',{1,2,3,m=4},{10,20,30,m=40}) is {11,22,23,m=44}
  function tablex.map2 (fun,t1,t2,...)
      assert_arg_iterable(1,t1)
      assert_arg_iterable(2,t2)
      fun = function_arg(1,fun)
      local res = {}
      for k,v in pairs(t1) do
          res[k] = fun(v,t2[k],...)
      end
      return setmeta(res,t1,List)
  end

  --- apply a function to values from two arrays.
  -- The result will be the length of the shortest array.
  -- @within MappingAndFiltering
  -- @func fun a function of at least two arguments
  -- @array t1 a list-like table
  -- @array t2 a list-like table
  -- @param ... extra arguments
  -- @usage imap2('+',{1,2,3,m=4},{10,20,30,m=40}) is {11,22,23}
  function tablex.imap2 (fun,t1,t2,...)
      assert_arg_indexable(2,t1)
      assert_arg_indexable(3,t2)
      fun = function_arg(1,fun)
      local res,n = {},math.min(#t1,#t2)
      for i = 1,n do
          res[i] = fun(t1[i],t2[i],...)
      end
      return res
  end

  --- 'reduce' a list using a binary function.
  -- @func fun a function of two arguments
  -- @array t a list-like table
  -- @return the result of the function
  -- @usage reduce('+',{1,2,3,4}) == 10
  function tablex.reduce (fun,t)
      assert_arg_indexable(2,t)
      fun = function_arg(1,fun)
      local n = #t
      local res = t[1]
      for i = 2,n do
          res = fun(res,t[i])
      end
      return res
  end

  --- apply a function to all elements of a table.
  -- The arguments to the function will be the value,
  -- the key and _finally_ any extra arguments passed to this function.
  -- Note that the Lua 5.0 function table.foreach passed the _key_ first.
  -- @within Iterating
  -- @tab t a table
  -- @func fun a function with at least one argument
  -- @param ... extra arguments
  function tablex.foreach(t,fun,...)
      assert_arg_iterable(1,t)
      fun = function_arg(2,fun)
      for k,v in pairs(t) do
          fun(v,k,...)
      end
  end

  --- apply a function to all elements of a list-like table in order.
  -- The arguments to the function will be the value,
  -- the index and _finally_ any extra arguments passed to this function
  -- @within Iterating
  -- @array t a table
  -- @func fun a function with at least one argument
  -- @param ... optional arguments
  function tablex.foreachi(t,fun,...)
      assert_arg_indexable(1,t)
      fun = function_arg(2,fun)
      for i = 1,#t do
          fun(t[i],i,...)
      end
  end

  --- Apply a function to a number of tables.
  -- A more general version of map
  -- The result is a table containing the result of applying that function to the
  -- ith value of each table. Length of output list is the minimum length of all the lists
  -- @within MappingAndFiltering
  -- @func fun a function of n arguments
  -- @tab ... n tables
  -- @usage mapn(function(x,y,z) return x+y+z end, {1,2,3},{10,20,30},{100,200,300}) is {111,222,333}
  -- @usage mapn(math.max, {1,20,300},{10,2,3},{100,200,100}) is  {100,200,300}
  -- @param fun A function that takes as many arguments as there are tables
  function tablex.mapn(fun,...)
      fun = function_arg(1,fun)
      local res = {}
      local lists = {...}
      local minn = 1e40
      for i = 1,#lists do
          minn = min(minn,#(lists[i]))
      end
      for i = 1,minn do
          local args,k = {},1
          for j = 1,#lists do
              args[k] = lists[j][i]
              k = k + 1
          end
          res[#res+1] = fun(unpack(args))
      end
      return res
  end

  --- call the function with the key and value pairs from a table.
  -- The function can return a value and a key (note the order!). If both
  -- are not nil, then this pair is inserted into the result. If only value is not nil, then
  -- it is appended to the result.
  -- @within MappingAndFiltering
  -- @func fun A function which will be passed each key and value as arguments, plus any extra arguments to pairmap.
  -- @tab t A table
  -- @param ... optional arguments
  -- @usage pairmap(function(k,v) return v end,{fred=10,bonzo=20}) is {10,20} _or_ {20,10}
  -- @usage pairmap(function(k,v) return {k,v},k end,{one=1,two=2}) is {one={'one',1},two={'two',2}}
  function tablex.pairmap(fun,t,...)
      assert_arg_iterable(1,t)
      fun = function_arg(1,fun)
      local res = {}
      for k,v in pairs(t) do
          local rv,rk = fun(k,v,...)
          if rk then
              res[rk] = rv
          else
              res[#res+1] = rv
          end
      end
      return res
  end

  local function keys_op(i,v) return i end

  --- return all the keys of a table in arbitrary order.
  -- @within Extraction
  --  @tab t A table
  function tablex.keys(t)
      assert_arg_iterable(1,t)
      return makelist(tablex.pairmap(keys_op,t))
  end

  local function values_op(i,v) return v end

  --- return all the values of the table in arbitrary order
  -- @within Extraction
  --  @tab t A table
  function tablex.values(t)
      assert_arg_iterable(1,t)
      return makelist(tablex.pairmap(values_op,t))
  end

  local function index_map_op (i,v) return i,v end

  --- create an index map from a list-like table. The original values become keys,
  -- and the associated values are the indices into the original list.
  -- @array t a list-like table
  -- @return a map-like table
  function tablex.index_map (t)
      assert_arg_indexable(1,t)
      return setmetatable(tablex.pairmap(index_map_op,t),Map)
  end

  local function set_op(i,v) return true,v end

  --- create a set from a list-like table. A set is a table where the original values
  -- become keys, and the associated values are all true.
  -- @array t a list-like table
  -- @return a set (a map-like table)
  function tablex.makeset (t)
      assert_arg_indexable(1,t)
      return setmetatable(tablex.pairmap(set_op,t),Set)
  end

  --- combine two tables, either as union or intersection. Corresponds to
  -- set operations for sets () but more general. Not particularly
  -- useful for list-like tables.
  -- @within Merging
  -- @tab t1 a table
  -- @tab t2 a table
  -- @bool dup true for a union, false for an intersection.
  -- @usage merge({alice=23,fred=34},{bob=25,fred=34}) is {fred=34}
  -- @usage merge({alice=23,fred=34},{bob=25,fred=34},true) is {bob=25,fred=34,alice=23}
  -- @see tablex.index_map
  function tablex.merge (t1,t2,dup)
      assert_arg_iterable(1,t1)
      assert_arg_iterable(2,t2)
      local res = {}
      for k,v in pairs(t1) do
          if dup or t2[k] then res[k] = v end
      end
      if dup then
        for k,v in pairs(t2) do
          res[k] = v
        end
      end
      return setmeta(res,t1,Map)
  end

  --- a new table which is the difference of two tables.
  -- With sets (where the values are all true) this is set difference and
  -- symmetric difference depending on the third parameter.
  -- @within Merging
  -- @tab s1 a map-like table or set
  -- @tab s2 a map-like table or set
  -- @bool symm symmetric difference (default false)
  -- @return a map-like table or set
  function tablex.difference (s1,s2,symm)
      assert_arg_iterable(1,s1)
      assert_arg_iterable(2,s2)
      local res = {}
      for k,v in pairs(s1) do
          if s2[k] == nil then res[k] = v end
      end
      if symm then
          for k,v in pairs(s2) do
              if s1[k] == nil then res[k] = v end
          end
      end
      return setmeta(res,s1,Map)
  end

  --- A table where the key/values are the values and value counts of the table.
  -- @array t a list-like table
  -- @func cmp a function that defines equality (otherwise uses ==)
  -- @return a map-like table
  -- @see seq.count_map
  function tablex.count_map (t,cmp)
      assert_arg_indexable(1,t)
      local res,mask = {},{}
      cmp = function_arg(2,cmp)
      local n = #t
      for i = 1,#t do
          local v = t[i]
          if not mask[v] then
              mask[v] = true
              -- check this value against all other values
              res[v] = 1  -- there's at least one instance
              for j = i+1,n do
                  local w = t[j]
                  local ok
                  if cmp then
                      ok = cmp(v,w)
                  else
                      ok = v == w
                  end
                  if ok then
                      res[v] = res[v] + 1
                      mask[w] = true
                  end
              end
          end
      end
      return setmetatable(res,Map)
  end

  --- filter an array's values using a predicate function
  -- @within MappingAndFiltering
  -- @array t a list-like table
  -- @func pred a boolean function
  -- @param arg optional argument to be passed as second argument of the predicate
  function tablex.filter (t,pred,arg)
      assert_arg_indexable(1,t)
      pred = function_arg(2,pred)
      local res,k = {},1
      for i = 1,#t do
          local v = t[i]
          if pred(v,arg) then
              res[k] = v
              k = k + 1
          end
      end
      return setmeta(res,t,List)
  end

  --- return a table where each element is a table of the ith values of an arbitrary
  -- number of tables. It is equivalent to a matrix transpose.
  -- @within Merging
  -- @usage zip({10,20,30},{100,200,300}) is {{10,100},{20,200},{30,300}}
  -- @array ... arrays to be zipped
  function tablex.zip(...)
      return tablex.mapn(function(...) return {...} end,...)
  end

  local _copy
  function _copy (dest,src,idest,isrc,nsrc,clean_tail)
      idest = idest or 1
      isrc = isrc or 1
      local iend
      if not nsrc then
          nsrc = #src
          iend = #src
      else
          iend = isrc + min(nsrc-1,#src-isrc)
      end
      if dest == src then -- special case
          if idest > isrc and iend >= idest then -- overlapping ranges
              src = tablex.sub(src,isrc,nsrc)
              isrc = 1; iend = #src
          end
      end
      for i = isrc,iend do
          dest[idest] = src[i]
          idest = idest + 1
      end
      if clean_tail then
          tablex.clear(dest,idest)
      end
      return dest
  end

  --- copy an array into another one, clearing `dest` after `idest+nsrc`, if necessary.
  -- @within Copying
  -- @array dest a list-like table
  -- @array src a list-like table
  -- @int[opt=1] idest where to start copying values into destination
  -- @int[opt=1] isrc where to start copying values from source
  -- @int[opt=#src] nsrc number of elements to copy from source
  function tablex.icopy (dest,src,idest,isrc,nsrc)
      assert_arg_indexable(1,dest)
      assert_arg_indexable(2,src)
      return _copy(dest,src,idest,isrc,nsrc,true)
  end

  --- copy an array into another one.
  -- @within Copying
  -- @array dest a list-like table
  -- @array src a list-like table
  -- @int[opt=1] idest where to start copying values into destination
  -- @int[opt=1] isrc where to start copying values from source
  -- @int[opt=#src] nsrc number of elements to copy from source
  function tablex.move (dest,src,idest,isrc,nsrc)
      assert_arg_indexable(1,dest)
      assert_arg_indexable(2,src)
      return _copy(dest,src,idest,isrc,nsrc,false)
  end

  function tablex._normalize_slice(self,first,last)
    local sz = #self
    if not first then first=1 end
    if first<0 then first=sz+first+1 end
    -- make the range _inclusive_!
    if not last then last=sz end
    if last < 0 then last=sz+1+last end
    return first,last
  end

  --- Extract a range from a table, like  'string.sub'.
  -- If first or last are negative then they are relative to the end of the list
  -- eg. sub(t,-2) gives last 2 entries in a list, and
  -- sub(t,-4,-2) gives from -4th to -2nd
  -- @within Extraction
  -- @array t a list-like table
  -- @int first An index
  -- @int last An index
  -- @return a new List
  function tablex.sub(t,first,last)
      assert_arg_indexable(1,t)
      first,last = tablex._normalize_slice(t,first,last)
      local res={}
      for i=first,last do append(res,t[i]) end
      return setmeta(res,t,List)
  end

  --- set an array range to a value. If it's a function we use the result
  -- of applying it to the indices.
  -- @array t a list-like table
  -- @param val a value
  -- @int[opt=1] i1 start range
  -- @int[opt=#t] i2 end range
  function tablex.set (t,val,i1,i2)
      assert_arg_indexable(1,t)
      i1,i2 = i1 or 1,i2 or #t
      if types.is_callable(val) then
          for i = i1,i2 do
              t[i] = val(i)
          end
      else
          for i = i1,i2 do
              t[i] = val
          end
      end
  end

  --- create a new array of specified size with initial value.
  -- @int n size
  -- @param val initial value (can be `nil`, but don't expect `#` to work!)
  -- @return the table
  function tablex.new (n,val)
      local res = {}
      tablex.set(res,val,1,n)
      return res
  end

  --- clear out the contents of a table.
  -- @array t a list
  -- @param istart optional start position
  function tablex.clear(t,istart)
      istart = istart or 1
      for i = istart,#t do remove(t) end
  end

  --- insert values into a table.
  -- similar to `table.insert` but inserts values from given table `values`,
  -- not the object itself, into table `t` at position `pos`.
  -- @within Copying
  -- @array t the list
  -- @int[opt] position (default is at end)
  -- @array values
  function tablex.insertvalues(t, ...)
      assert_arg(1,t,'table')
      local pos, values
      if select('#', ...) == 1 then
          pos,values = #t+1, ...
      else
          pos,values = ...
      end
      if #values > 0 then
          for i=#t,pos,-1 do
              t[i+#values] = t[i]
          end
          local offset = 1 - pos
          for i=pos,pos+#values-1 do
              t[i] = values[i + offset]
          end
      end
      return t
  end

  --- remove a range of values from a table.
  -- End of range may be negative.
  -- @array t a list-like table
  -- @int i1 start index
  -- @int i2 end index
  -- @return the table
  function tablex.removevalues (t,i1,i2)
      assert_arg(1,t,'table')
      i1,i2 = tablex._normalize_slice(t,i1,i2)
      for i = i1,i2 do
          remove(t,i1)
      end
      return t
  end

  local _find
  _find = function (t,value,tables)
      for k,v in pairs(t) do
          if v == value then return k end
      end
      for k,v in pairs(t) do
          if not tables[v] and type(v) == 'table' then
              tables[v] = true
              local res = _find(v,value,tables)
              if res then
                  res = tostring(res)
                  if type(k) ~= 'string' then
                      return '['..k..']'..res
                  else
                      return k..'.'..res
                  end
              end
          end
      end
  end

  --- find a value in a table by recursive search.
  -- @within Finding
  -- @tab t the table
  -- @param value the value
  -- @array[opt] exclude any tables to avoid searching
  -- @usage search(_G,math.sin,{package.path}) == 'math.sin'
  -- @return a fieldspec, e.g. 'a.b' or 'math.sin'
  function tablex.search (t,value,exclude)
      assert_arg_iterable(1,t)
      local tables = {[t]=true}
      if exclude then
          for _,v in pairs(exclude) do tables[v] = true end
      end
      return _find(t,value,tables)
  end

  --- return an iterator to a table sorted by its keys
  -- @within Iterating
  -- @tab t the table
  -- @func f an optional comparison function (f(x,y) is true if x < y)
  -- @usage for k,v in tablex.sort(t) do print(k,v) end
  -- @return an iterator to traverse elements sorted by the keys
  function tablex.sort(t,f)
      local keys = {}
      for k in pairs(t) do keys[#keys + 1] = k end
      tsort(keys,f)
      local i = 0
      return function()
          i = i + 1
          return keys[i], t[keys[i]]
      end
  end

  --- return an iterator to a table sorted by its values
  -- @within Iterating
  -- @tab t the table
  -- @func f an optional comparison function (f(x,y) is true if x < y)
  -- @usage for k,v in tablex.sortv(t) do print(k,v) end
  -- @return an iterator to traverse elements sorted by the values
  function tablex.sortv(t,f)
      local rev = {}
      for k,v in pairs(t) do rev[v] = k end
      local next = tablex.sort(rev,f)
      return function()
          local value,key = next()
          return key,value
      end
  end

  --- modifies a table to be read only.
  -- This only offers weak protection. Tables can still be modified with
  -- `table.insert` and `rawset`.
  -- @tab t the table
  -- @return the table read only.
  function tablex.readonly(t)
      local mt = {
          __index=t,
          __newindex=function(t, k, v) error("Attempt to modify read-only table", 2) end,
          __pairs=function() return pairs(t) end,
          __ipairs=function() return ipairs(t) end,
          __len=function() return #t end,
          __metatable=false
      }
      return setmetatable({}, mt)
  end
end

-------------------------------------------------------------------------------
--- Penlight (https://github.com/stevedonovan/Penlight/)
---       Pretty (Write Only + keywords from lexer)
-------------------------------------------------------------------------------

local pretty = {}
do
  local keywords = {
    ["and"] = true, ["break"] = true,  ["do"] = true,
    ["else"] = true, ["elseif"] = true, ["end"] = true,
    ["false"] = true, ["for"] = true, ["function"] = true,
    ["if"] = true, ["in"] = true,  ["local"] = true, ["nil"] = true,
    ["not"] = true, ["or"] = true, ["repeat"] = true,
    ["return"] = true, ["then"] = true, ["true"] = true,
    ["until"] = true,  ["while"] = true
  }


  --- Create a string representation of a Lua table.
  --  This function never fails, but may complain by returning an
  --  extra value. Normally puts out one item per line, using
  --  the provided indent; set the second parameter to '' if
  --  you want output on one line.
  --  @param tbl {table} Table to serialize to a string.
  --  @param space {string} (optional) The indent to use.
  --  Defaults to two spaces; make it the empty string for no indentation
  --  @param not_clever {bool} (optional) Use for plain output, e.g {['key']=1}.
  --  Defaults to false.
  --  @return a string
  --  @return a possible error message
  function pretty.write(tbl,space,not_clever)
      if type(tbl) ~= 'table' then
          local res = tostring(tbl)
          if type(tbl) == 'string' then return quote(tbl) end
          return res, 'not a table'
      end
      local set = ' = '
      if space == '' then set = '=' end
      space = space or '  '
      local lines = {}
      local line = ''
      local tables = {}


      local function put(s)
          if #s > 0 then
              line = line..s
          end
      end

      local function putln (s)
          if #line > 0 then
              line = line..s
              append(lines,line)
              line = ''
          else
              append(lines,s)
          end
      end

      local function eat_last_comma ()
          local n,lastch = #lines
          local lastch = lines[n]:sub(-1,-1)
          if lastch == ',' then
              lines[n] = lines[n]:sub(1,-2)
          end
      end


      local writeit
      writeit = function (t,oldindent,indent)
          local tp = type(t)
          if tp ~= 'string' and  tp ~= 'table' then
              putln(quote_if_necessary(tostring(t))..',')
          elseif tp == 'string' then
              if t:find('\n') then
                  putln('[[\n'..t..']],')
              else
                  putln(quote(t)..',')
              end
          elseif tp == 'table' then
              if tables[t] then
                  putln('<cycle>,')
                  return
              end
              tables[t] = true
              local newindent = indent..space
              putln('{')
              local used = {}
              if not not_clever then
                  for i,val in ipairs(t) do
                      put(indent)
                      writeit(val,indent,newindent)
                      used[i] = true
                  end
              end
              for key,val in pairs(t) do
                  local numkey = type(key) == 'number'
                  if not_clever then
                      key = tostring(key)
                      put(indent..index(numkey,key)..set)
                      writeit(val,indent,newindent)
                  else
                      if not numkey or not used[key] then -- non-array indices
                          if numkey or not is_identifier(key) then
                              key = index(numkey,key)
                          end
                          put(indent..key..set)
                          writeit(val,indent,newindent)
                      end
                  end
              end
              tables[t] = nil
              eat_last_comma()
              putln(oldindent..'},')
          else
              putln(tostring(t)..',')
          end
      end
      writeit(tbl,'',space)
      eat_last_comma()
      return concat(lines,#space > 0 and '\n' or '')
  end
end

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
    local string =  s('output.failure') .. ': ' ..
    failure.elementTrace.short_src .. ' @ ' ..
    failure.elementTrace.currentline ..
    '\n' .. getFullName(failure) .. '\n\n'

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
      elementTrace = element.trace or debug,
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
--- Olivine-Labs Busted - XML Test File
-------------------------------------------------------------------------------

local XMLTests
do
  local ret = {}
  local getTrace =  function(filename, info)
    local index = info.traceback:find('\n%s*%[C]')
    info.traceback = info.traceback:sub(1, index)
    return info, false
  end

  ret.load = function(busted, filename)
    local testfunc, teststring

    local XMLTable = XmlDoc.CreateFromFile(filename):ToTable()
    if not XMLTable or XMLTable.__XmlNode ~= "Definition" or
       not XMLTable[1] or not XMLTable[1].__XmlText then
      return
    end
    teststring = XMLTable[1].__XmlText

    local success, err = pcall(function()
      testfunc, err = loadstring(teststring)

      if not testfunc then
        busted.publish({ 'error', 'file' }, filename, nil, nil, err)
      end
    end)

    if not success then
      busted.publish({ 'error', 'file' }, filename, nil, nil, err)
    end

    return testfunc, getTrace
  end
  XMLTests = ret
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

local function ExecuteTests()
  busted.publish({ 'suite', 'start' })
  busted.execute()
  busted.publish({ 'suite', 'end' })
	busted.context.reset()
end

local loaders = {
	["lua"] = LuaTests,
	["xml"] = XMLTests,
}

local function RunTest(self, strTest)
  local strFile = BustedTests[self][strTest]
  if not strFile then return end
	local loader = loaders[string.sub(strFile,-3)]
	if not loader then return end
  local testFile, getTrace = loader.load(busted, strFile)

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
    local loader = loaders[string.sub(v, -3)]
    if loader then
      local testFile, getTrace = loader.load(busted, v)

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

function busted:Register(oAddon)
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

function busted:OnDependencyError(strDep, strError)
	return false
end

function busted:OnLoad()
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

  spy.init()
  stub.init()

  bustedInit(busted)

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

Apollo.RegisterPackage(busted, MAJOR, MINOR, {"Lib:Assert-1.0"})
