describe("busted", function()
	local obj1, obj2
	local util

	setup(function()
		util = "Something you setup for once"
	end)

	teardown(function()
		util = nil
	end)

	before_each(function()
		obj1 = { test = "yes" }
		obj2 = { test = "yes" }
	end)

	it("sets up vars with the before_each", function()
		obj2 = { test = "no" }
		assert.are_not.same(obj1, obj2)
	end)

	it("sets up vars with the before_each", function()
		-- obj2 is reset thanks to the before_each
		assert.same(obj1, obj2)
	end)

	describe("nested", function()
		it("also runs the before_each here", function()
		-- if this describe also had a before_each, it would run
		-- both, starting with the parents'. You can go n-deep.
		end)
	end)
end)