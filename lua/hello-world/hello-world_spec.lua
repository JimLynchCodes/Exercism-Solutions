local hello_world = require('hello-world')

describe('hello-world', function()
  it('says hello world', function()
    local result = hello_world.hello()
    assert.are.equal('Hello, World!', result)
  end)
end)
