module hello_world;

string hello()
{
    return "Hello";
}

unittest
{
    assert(hello() == "Hello, World!");
}
