# Hello World

The classical introductory exercise. Just say "Hello, World!".

["Hello, World!"](http://en.wikipedia.org/wiki/%22Hello,_world!%22_program) is
the traditional first program for beginning programming in a new language
or environment.

The objectives are simple:

- Write a function that returns the string "Hello, World!".
- Run the test suite and make sure that it succeeds.
- Submit your solution and check it at the website.

If everything goes well, you will be ready to fetch your first real exercise.

### Project Structure

Clojurescript Exercises in Exercism use [Clojure CLI](https://clojure.org/guides/getting_started#_clojure_installer_and_cli_tools) to configure and run your code and use some clojure/script dependencies to manage test process.

You will find a test file named `hello_world_test.cljs` inside `test` directory.
Write your code in `src/hello_world.cljs`. It should use the namespace `hello-world` so that tests can pick it up.

### Running tests

Run the tests using `clj -A:test` command and make them pass:

```
$ clj -A:test

Testing hello-world-test

Ran 1 tests containing 1 assertions.
0 failures, 0 errors.
```

Then submit the exercise using:

```
$ exercism submit src/hello_world.cljs
```

For more detailed instructions and learning resources refer [Exercism's Clojurescript Language Page](http://exercism.io/languages/clojurescript).

## Source

This is an exercise to introduce users to using Exercism [http://en.wikipedia.org/wiki/%22Hello,_world!%22_program](http://en.wikipedia.org/wiki/%22Hello,_world!%22_program)

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
