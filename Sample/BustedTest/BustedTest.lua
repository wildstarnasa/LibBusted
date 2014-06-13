-----------------------------------------------------------------------------------------------
-- BustedTest
-----------------------------------------------------------------------------------------------

require "Window"
require "GameLib"
require "Apollo"
require "Spell"
require "Unit"
require "PublicEventsLib"
require "ChallengesLib"
require "PlayerPathLib"
require "PathMission"

-----------------------------------------------------------------------------------------------
-- BustedTest Module Definition
-----------------------------------------------------------------------------------------------
local BustedTest = {} 

-----------------------------------------------------------------------------------------------
-- Constants
-----------------------------------------------------------------------------------------------
-- e.g. local kiExampleVariableMax = 999


-----------------------------------------------------------------------------------------------
-- Initialization
-----------------------------------------------------------------------------------------------
function BustedTest:new(o)
    o = o or {}
    setmetatable(o, self)
    self.__index = self 

    self.tSavedData={}
    self.tSavedData["v"]=0

    -- initialize variables here

    return o
end

function BustedTest:Init()
    Apollo.RegisterAddon(self, false, "", {"Lib:Assert-1.0", "Lib:Busted-2.0"})
end

function BustedTest:OnLoad()
    Apollo.GetPackage("Lib:Busted-2.0").tPackage:Register(self)
    self.xmlDoc = XmlDoc.CreateFromFile("BustedTest.xml")
    self.xmlDoc:RegisterCallback("OnDocumentReady", self)
end

function BustedTest:OnDependencyError(strDep, strError)
    return false
end

function BustedTest:OnDocumentReady()
    self.wndMain = Apollo.LoadForm(self.xmlDoc, "BustedTestForm", nil, self)

    local wndContainer = self.wndMain:FindChild("TestsContainer")
    for k,v in self:IterateTests() do
        local wndTest = Apollo.LoadForm(self.xmlDoc, "TestButton", wndContainer, self)
        wndTest:FindChild("TestName"):SetText(k)
    end
    wndContainer:ArrangeChildrenVert(0)
end

-----------------------------------------------------------------------------------------------
-- BustedTestForm Functions
-----------------------------------------------------------------------------------------------
-- when the OK button is clicked
function BustedTest:OnOK()
  self.wndMain:Show(false) -- hide the window
end

-- when the Cancel button is clicked
function BustedTest:OnCancel()
  self.wndMain:Show(false) -- hide the window
end

function BustedTest:OnTest(wndHandler, wndControl, eButton)
    self:RunTest(wndControl:FindChild("TestName"):GetText())
end

function BustedTest:OnTestAll(wndHandler, wndControl, eButton)
    self:RunTests()
end

-----------------------------------------------------------------------------------------------
-- Jabbithole Instance
-----------------------------------------------------------------------------------------------
local BustedTestInst = BustedTest:new()
BustedTestInst:Init()