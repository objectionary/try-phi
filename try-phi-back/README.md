# Try-phi backend

## Clone
```sh
git clone --recurse-submodules -j8 https://github.com/br4ch1st0chr0n3/try-phi-back
cd try-phi-back
```

## Run
```sh
sh build.sh
```

## Deploy to Heroku?
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

* `push` and get

```sh
2022-06-25T14:39:52.239150+00:00 heroku[web.1]: State changed from crashed to starting
2022-06-25T14:39:53.260912+00:00 heroku[web.1]: Starting process with command `try-servant-exe`
2022-06-25T14:40:53.904920+00:00 heroku[web.1]: Error R10 (Boot timeout) -> Web process failed to bind to $PORT within 60 seconds of launch
2022-06-25T14:40:53.936273+00:00 heroku[web.1]: Stopping process with SIGKILL
2022-06-25T14:40:54.126423+00:00 heroku[web.1]: Process exited with status 137
2022-06-25T14:40:54.215588+00:00 heroku[web.1]: State changed from starting to crashed

```