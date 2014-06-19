describe("some assertions", function()
	it("tests positive assertions", function()
		assert.is_true(true)  -- Lua keyword chained with _
		assert.True(true)     -- Lua keyword using a capital
		assert.are.equal(1, 1)
		assert.has.errors(function() error("this should fail") end)
	end)

	it("tests negative assertions", function()
		assert.is_not_true(false)
		assert.are_not.equals(1, "1")
		assert.has_no.errors(function() end)
	end)
end)

describe("some asserts", function()
	it("checks if they're equals", function()
		local expected = 1
		local obj = expected

		assert.are.equals(expected, obj)
	end)
end)

describe("some asserts", function()
	it("checks if they're the same", function()
		local expected = { name = "Jack" }
		local obj = { name = "Jack" }

		assert.are.same(expected, obj)
	end)
end)

describe("some asserts", function()
	it("checks true", function()
		assert.is_true(true)
		assert.is.not_true("Yes")
		assert.is.truthy("Yes")
	end)

	it("checks false", function()
		assert.is_false(false)
		assert.is.not_false(nil)
		assert.is.falsy(nil)
	end)
end)

describe("some asserts", function()
	it("should throw an error", function()
		assert.has_error(function() error("Yup,  it errored") end)
		assert.has_no.errors(function() end)
	end)

	it("should throw the error we expect", function()
		local errfn = function()
			error("DB CONN ERROR")
		end

		assert.has_error(errfn, "DB CONN ERROR")
	end)
end)

local function has_property(state, arguments)
	local has_key = false

	if not type(arguments[1]) == "table" or #arguments ~= 2 then
		return false
	end

	for key, value in pairs(arguments[1]) do
		if key == arguments[2] then
			has_key = true
		end
	end

	-- state.mod holds true or false, which is true normally, or false if we
	-- are negating the assertion by using is_not or one of its aliases.

	return state.mod == has_key
end
feeBin()
s:set("assertion.has_property.positive", "Expected property %s in:\n%s")
s:set("assertion.has_property.negative", "Expected property %s to not be in:\n%s")
assert:register("assertion", "has_property", has_property, "assertion.has_property.positive", "assertion.has_property.negative")
describe("my table", function()
	it("has a name property", function()
		assert.has_property({ name = "Jack" }, "name")
	end)
end)