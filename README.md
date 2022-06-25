# try-servant

## Deploy to Heroku
* Read [this](https://hackernoon.com/for-all-the-world-to-see-deploying-haskell-with-heroku-7ea46f827ce) till `web: run-server`

* Select a port when running locally:
```haskell
startApp :: IO ()
startApp = catchIOError 
  (do
    port <- read <$> getEnv "Port"
    print port
    run port app)
  (\_ -> run 8082 app)
```

* Next, add a `Procfile` and [commit](https://devcenter.heroku.com/articles/procfile#deploying-to-heroku) it to `heroku`

* Return to the [guide](https://hackernoon.com/for-all-the-world-to-see-deploying-haskell-with-heroku-7ea46f827ce)
