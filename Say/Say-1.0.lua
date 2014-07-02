-------------------------------------------------------------------------------
--- Olivine-Labs Say
-------------------------------------------------------------------------------

local MAJOR, MINOR = "Olivine:Say-1.0", 2
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion or 0) >= MINOR then
    return
end
-- Set a reference to the actual package or create an empty table
local s = APkg and APkg.tPackage or {}

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

function s:OnLoad() end

Apollo.RegisterPackage(s, MAJOR, MINOR, {})
