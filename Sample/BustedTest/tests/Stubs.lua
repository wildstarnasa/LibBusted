describe("stubs", function()
	it("replaces an original function", function()
		local t = {
			greet = function(msg) print(msg) end
		}

		stub(t, "greet")

		t.greet("Hey!") -- DOES NOT print 'Hey!'
		assert.stub(t.greet).was.called_with("Hey!")

		t.greet:revert()  -- reverts the stub
		t.greet("Hey!") -- DOES print 'Hey!'
	end)
end)