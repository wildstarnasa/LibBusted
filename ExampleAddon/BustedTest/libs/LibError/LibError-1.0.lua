-- LibError incorporates some ideas from !Buggrabber
--
-- The BugSack and !BugGrabber team is:
-- Current Developer: Funkydude, Rabbit
-- Past Developers: Rowne, Ramble, industrial, Fritti, kergoth, ckknight
-- Testers: Ramble, Sariash
--
--[[
!BugGrabber, World of Warcraft addon that catches errors and formats them with a debug stack.
Copyright (C) 2013 The !BugGrabber Team

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

]]

-- Adapted to WildStar Packaging format by Sinaloit
local MAJOR, MINOR = "Gemini:LibError-1.0", 5
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion > 0) then
	return -- no upgrades
end
-- Set a reference to the actual package or create an empty table
local LibError = APkg and APkg.tPackage or {}

-----------------------------------------------------------------------
-- local-ization, mostly for use with the FindGlobals script to catch
-- misnamed variable names. We're not hugely concerned with performance.

local _G = _G
local type, table, next =
      type, table, next
local tostring, tonumber, GetTime =
      tostring, tonumber, os.clock
-- GLOBALS: date

-----------------------------------------------------------------------
-- Config variables
--
local MAX_LIBERROR_ERRORS = 1000

-- If we get more errors than this per second, we stop all capturing
--
local LIBERROR_ERRORS_PER_SEC_BEFORE_THROTTLE = 10

-----------------------------------------------------------------------
-- Locals
--
local displayObjectName
-- Shorthand to LibErrorDB.errors
local db

-- Errors we catch during the addon loading process, before we can
-- directly display errors.
local errorQueue = {}
local bFullyLoaded
local paused

-----------------------------------------------------------------------
-- Localization
--
local ktLocales = {
	[1] = "enUS",
	[2] = "deDE",
	[3] = "frFR",
	[4] = "koKR",
}

local L = {
	LIBERROR_STOPPED = "There are too many errors in your UI. As a result, your game experience may be degraded. Disable or update the failing addons if you don't want to see this message again.",
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
	-- return ktLocales[(Apollo.GetConsoleVariable("locale.languageId") or 1)]
end

-----------------------------------------------------------------------
-- Utility
--
function OnError(tErrorObject)
	local tAddon, tADI = Apollo.GetAddon(tErrorObject.strName), nil
	local strError = tErrorObject.message
	if tAddon ~= nil then
		tADI = Apollo.GetAddonInfo(tErrorObject.strName)
		Apollo.AddAddonErrorText(tAddon, tErrorObject.message .. "\n" .. tErrorObject.stack)
	else
		tADI = {
			strName = tErrorObject.strName,
			strAuthor = "Unknown",
			arErrors = {tErrorObject.message},
		}
		strError = tErrorObject.message .. "\n" .. tErrorObject.stack
	end
	if not bFullyLoaded then
		errorQueue = errorQueue or {}
		tADI.strError = strError
		table.insert(errorQueue, tADI)
	else
		Event_FireGenericEvent("LuaError", tADI, strError, true)
	end
end

local function debugLocals(nLevel)
	local tVars, nIdx = {}, 1
	while true do
		local ln, lv = debug.getlocal(nLevel, nIdx)
		if ln ~= nil then
			tVars[ln] = lv
		else
			break
		end
		nIdx = nIdx + 1
	end
	return tVars
end

-- Error handler
local grabError
do
	--local tmp = {}
	local msgsAllowed = LIBERROR_ERRORS_PER_SEC_BEFORE_THROTTLE
	local msgsAllowedLastTime = GetTime()
	local lastWarningTime = 0
	function grabError(errorMessage)
		-- Flood protection --
		msgsAllowed = msgsAllowed + (GetTime()-msgsAllowedLastTime)*LIBERROR_ERRORS_PER_SEC_BEFORE_THROTTLE
		msgsAllowedLastTime = GetTime()
		if msgsAllowed < 1 then
			if not paused then
				if GetTime() > lastWarningTime + 10 then
					Print(L.LIBERROR_STOPPED)
					lastWarningTime = GetTime()
				end
				paused=true
			end
			return
		end
		paused=false
		if msgsAllowed > LIBERROR_ERRORS_PER_SEC_BEFORE_THROTTLE then
			msgsAllowed = LIBERROR_ERRORS_PER_SEC_BEFORE_THROTTLE
		end
		msgsAllowed = msgsAllowed - 1

		-- Grab it --
		errorMessage = tostring(errorMessage)

		local looping = errorMessage:find("LibError") and true or nil
		if looping then
			Print(errorMessage)
			return
		end

		local errorObject = found

		if not errorObject then
			local strStack = debug.traceback()
			local tLocals = debugLocals(4)

			-- Store the error
			errorObject = {
				message = errorMessage,
				stack = strStack,
				locals = tLocals,
				strName = tLocals.self and tostring(tLocals.self) or "Unknown",
				time = os.date("%Y/%m/%d %H:%M:%S"),
				counter = 1,
			}

		end

		OnError(errorObject)
	end
end
LibError.Error = grabError

function LibError:IsPaused() return paused end

function LibError:OnFullyLoaded()
	bFullyLoaded = true
	if type(errorQueue) ~= "table" then
		return
	end
	while #errorQueue > 0 do
		local tADIM = table.remove(errorQueue, 1)
		Event_FireGenericEvent("LuaError", tADIM, tADIM.strError, true)
	end
end

-- Init code
function LibError:OnLoad()
	Apollo.RegisterEventHandler("InterfaceMenuListHasLoaded", "OnFullyLoaded", self)
	if type(LibError.LoadTranslations) == "function" then
		local locale = GetLocale()
		if locale ~= "enUS" and locale ~= "enGB" then
			LibError:LoadTranslations(locale, L)
		end
		LibError.LoadTranslations = nil
	end
end
-- No dependencies
function LibError:OnDependencyError(strDep, strError)
	return false
end

function LibError:LoadTranslations(locale, L)
	if locale == "koKR" then
L["LIBERROR_STOPPED"] = "이것은 초당 %d개 이상의 오류를 발견하였기에 |cffffff7fLibError|r의 오류 캡쳐가 중지되었으며, 캡쳐는 %d초 후 재개됩니다." -- Needs review

	elseif locale == "deDE" then
L["LIBERROR_STOPPED"] = "In deinem UI treten zu viele Fehler auf, als Folge davon könnte dein Spiel langsamer laufen. Deaktiviere oder aktualisiere die fehlerhaften Addons, wenn du diese Meldung nicht mehr sehen willst."

	elseif locale == "esES" then
L["LIBERROR_STOPPED"] = "¡Hay demasiados errores en la interfaz! Esto puede afectar negativamente el rendimiento del juego. Desactivar o actualizar los addons que están causando los errores si no deseas ver este mensaje nunca más."

	elseif locale == "zhTW" then
L["LIBERROR_STOPPED"] = "你的UI有太多的錯誤。這可能導致糟糕的遊戲體驗。禁用或是更新錯誤的插件如果你不想看到再次看到這個訊息。"

	elseif locale == "zhCN" then
L["LIBERROR_STOPPED"] = "用户界面有太多的错误。所以，游戏体验会被降低。如不想再看到此信息请禁用或升级失效插件。"

	elseif locale == "ruRU" then
L["LIBERROR_STOPPED"] = "LibError прекратил захватывать ошибки, так как захватил более %d ошибок  в секунду. Захват возобновится через %d секунд." -- Needs review

	elseif locale == "frFR" then
L["LIBERROR_STOPPED"] = "LibError a cessé de capturer des erreurs, car plus de %d erreurs ont été capturées par seconde. La capture sera reprise dans %d secondes." -- Needs review

	elseif locale == "esMX" then
L["LIBERROR_STOPPED"] = "¡Hay demasiados errores en la interfaz! Esto puede afectar negativamente el rendimiento del juego. Desactivar o actualizar los addons que están causando los errores si no deseas ver este mensaje nunca más."

	elseif locale == "ptBR" then
L["LIBERROR_STOPPED"] = "LibError|r parou de capturar erros, já que capturou mais de %d erros por segundo. A captura será resumida em %d segundos." -- Needs review

	elseif locale == "itIT" then
L["LIBERROR_STOPPED"] = "LibErrorr ha smesso di catturare errori, poichè ha catturato più di %d errori al second. La cattura riprenderà tra %d secondi." -- Needs review
	end
end

Apollo.RegisterPackage(LibError, MAJOR, MINOR, {})