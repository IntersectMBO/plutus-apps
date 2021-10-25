## E2E UI Tests with Selenium
This is an end-to-end test suite using [hs-webdriver](https://hackage.haskell.org/package/webdriver), which is unfortuantely no longer maintained. I've not yet had any issues with issuing commands to selenium server, although there is one known limitation: I've not been successful integrating with Haskell Testing Frameworks [hspec-webdriver](https://hackage.haskell.org/package/hspec-webdriver) and [quickcheck-webdriver](https://hackage.haskell.org/package/quickcheck-webdriver) as they are quite old implementations and incompatible with recent ghc. Consequently, these tests all run synchronously in Main where a single assertion failure or other exception kills the app (suggestions welcome).

**RISK:** If this haskell webdriver library is not up to the task then we may want to either resurrect it, or use a python/java/javascript approach instead.

To run these tests locally you'll need Firefox and Java installed to start the Selenium webserver. See workflow for example commands. `xvfb` is used in CI to virtualise display, which is not necessary to run the Selenium webserver locally with this. That is, just run `java -jar selenium-server-4.0.0.jar standalone`.

All tests are defined with a behavioural-driven design pattern for readability.

### plutus-playground-e2e-tests
This suite depends on a local instances of the Plutus Playground server and client beign built and run with default configuration. It then proceeds to interact with the webapp making assertions to confirm correct script compilation and evaluation.
Currently, there are just two sanity tests to demonstrate this approach. `HelloWorldTest` and `CrowdFundingTest`.
