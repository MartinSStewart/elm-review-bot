# elm-review-bot

A bot that uses [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) to find common mistakes in Elm packages.

Currently the bot only checks if there are unused dependencies but some future checks might be:
- Check that the Elm version range in the elm.json file is valid
- Check that packages don't depend on packages that no longer exist
- Check that all the types used by exposed functions are also exposed

This bot is written 100% written using Elm and is hosted with Lamdera.