it('checks file contents',function()
	local sample = Apollo.LoadForm(self.xmlDoc, "SampleForm", nil, self)

	-- ensure that once test has finished sample:Destroy() is called
	-- independent of test outcome
	finally(function() sample:Destroy() end)

	it('does a thing', function()
		-- do things with the sample
	end)
end)
