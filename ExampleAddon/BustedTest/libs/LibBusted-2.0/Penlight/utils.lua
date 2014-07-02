local MAJOR, MINOR = "Lib:Penlight:Utils-1.0", 1
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion or 0) >= MINOR then
    return
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
local operator

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

function utils:OnLoad()
    operator = Apollo.GetPackage("Lib:Penlight:Operator-1.0").tPackage
end

Apollo.RegisterPackage(utils, MAJOR, MINOR, {"Lib:Penlight:Operator-1.0"})
