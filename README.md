# λGPT

## Implementation Overview & Architectural Choices
The target of this coursework was to create a natural language REPL interface using monadic parser combinators rather than LLMs. To ensure high maintainability and exceptional robustness, several architectural choices were made:
### Memory Management 
Rather than implementing a complex StateT monad transformer over IO, I chose explicit state passing using tail recursion within the runREPL loop (loop :: [(String, String)] -> IO ()). By explicitly connecting the associative list representing memory through the respondTo function, the codebase remains highly readable and adheres to functional purity principles without over engineering the solution.
### Parsing Strategy
A significant challenge in natural language parsing is to seperate different prefixes, such as distinguishing between "What is two plus two?" and "What is the weather like today?". I addressed this challenge through strategic use of Megaparsec’s try combinator, enabling the parser to safely backtrack without consuming input and failing permanently. Furthermore, to ensure a smooth user experience, the parsers were specifically designed to be robust against natural variations of the same input: I utilised Megaparsec's string' combinator to make the request matching case insensitive. Additionally, I integrated the optional combinator to catch and consume trailing punctuation (such as periods or question marks) alongside space to handle accidental trailing whitespaces, preventing strict parsing failures on valid inputs.
### Expression Evaluation & Error Handling
In order to evaluate mathematical operations, an Abstract Syntax Tree was built. Furthermore, to meet the left-to-right natural language evaluation requirement, a foldl accumulator in parseExpr was used. Lastly, the evaluateExpr function was designed to return Either String Int instead of an Int. This choice leverages the Either monad to beautifully catch erros, such as referencing the "that" keyword before any math has been performed, which will prevent runtime crashes and ensures the chatbot runs gracefully.
### Timezone Accuracy
While testing my chatbot at midnight(don't ask me why), I discovered a bug that was caused by using the wrong time zone: GMT rather than GMT+1 (BST). To prevent logic errors occurring late at night, I made use of Data.Time.LocalTime alongside getCurrentTimeZone. This ensures that date calculations for "today", "tomorrow", and "how long ago" remain accurate to the users' local clock despite having users from all across the world, rather than defaulting to UTC.

## Beyond the Specification: API Integration (Weather)
To extend the chatbot’s functionality, I integrated a live weather forecasting feature.
### Separation of Concerns
To maximise code elegance and readability, all external HTTP requests and JSON parsing were isolated into a dedicated module named LGPT.Weather. To support this architectural separation, I modified the package.yaml file to explicitly expose the new LGPT.Weather module and include the necessary aeson and http-conduit dependencies. This ensures that stack build compiles the isolated logic correctly while keeping TUI.hs strictly focused on user I/O and parsing, and enhances the maintainability of the code as APIs tend to be unreliable and separating them makes debugging easier. 
### Robust Network Handling
After a tedious trial-and-error, I specifically chose the Open-Meteo API as it is keyless, ensuring the project is cheap and highly portable and won't crash due to third-party rate limits during marking. Furthermore, I used the httpJSONEither function to fetch the data. If the user's network drops or the API fails, the application handles the Left error and returns a polite failure string, completely isolating the REPL crashes caused by the network.
New intermediate data types (OMCurrent, OMDaily) were merely introduced to safely separate the external JSON shape from the internal CurrentWeather representation, and a custom pattern matching function (wmoToDesc) was written to translate raw WMO numeric weather codes into user-friendly English descriptions.
### How to Test This Feature
To test this feature, simply run "stack build" to install all the requires libraries, then start the REPL using "stack run" to start the chatbot and type exactly: "What is the weather like today?" or "What is the weather like tomorrow?". It's worth mentioning that the parser also gracefully handles missing question marks. 

## Personal Experience
Coming from a background heavily focused on imperative and object-oriented paradigms in Python, Java, and C, developing in Haskell required a fundamental change in the way of thinking. Managing state immutably and thinking in terms of monadic composition initially presented a steep learning curve.
However, building the Abstract Syntax Tree for mathematical operations and wiring the parser combinators demonstrated the profound power of Haskell. Writing the same program in an imperative programming language would certainly result in a less elegant code and possibly require dozens of if-statements. The ability to safely catch edge cases (like the Either pattern for missing memory variables or failed JSON parses) without relying on traditional try/catch blocks highlighted the safety and elegance of strongly typed functional programming. It was a highly rewarding experience that fundamentally changed how I approach data pipelines and error handling.

## Resources Used
### Open-Meteo API Documentation
(https://open-meteo.com/en/docs) - Used to understand the JSON response structure, query parameters, and WMO weather codes for the custom weather integration.
