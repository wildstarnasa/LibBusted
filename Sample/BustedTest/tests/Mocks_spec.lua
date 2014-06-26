describe("mocks", function()
	it("replaces a table with spies", function()
		local t = {
			thing = function(msg) Print(msg) end
		}

		local m = mock(t) -- mocks the table with spies, so it will print

		m.thing("Coffee")
		assert.spy(m.thing).was.called_with("Coffee")
	end)

	it("replaces a table with stubs", function()
		local t = {
			thing = function(msg) Print(msg) end
		}

		local m = mock(t, true) -- mocks the table with stubs, so it will not print

		m.thing("Coffee")
		assert.stub(m.thing).was.called_with("Coffee")
	end)
end)
