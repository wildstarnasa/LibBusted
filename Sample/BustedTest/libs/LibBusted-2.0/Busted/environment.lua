local MAJOR, MINOR = "Olivine:Busted:Environment-2.0", 1
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion or 0) >= MINOR then
    return
end

-------------------------------------------------------------------------------
--- Olivine-Labs Busted-Environment
-------------------------------------------------------------------------------
LibEnvironment = APkg and APkg.tPackage or {}

LibEnvironment.Builder = function(context)

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

Apollo.RegisterPackage(LibEnvironment, MAJOR, MINOR, {})
