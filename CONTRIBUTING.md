# Contributing

Welcome to the [University of Warwick Tabletop and Roleplaying Society](https://warwicktabletop.co.uk)'s Discord server bot! You can join our Discord server [here](https://warwicktabletop.co.uk/discord)! You can access the channel for developer discussion by giving yourself the @computer_person role.

Please feel free to contribute to the code in whatever way you are able. We're more than happy to accept new code, bug fixes, documentation, issues, and anything else you think will improve the bot! If you do start work on an issue, please first let us know in the issue's thread or in our Discord server to avoid competing pull requests.

**Our society has a [Code of Conduct](https://www.warwicktabletop.co.uk/page/codeofconduct/). We expect it to be upheld by contributors in all our online spaces the same way we'd expect it to be upheld by attendees of our events.**

## What can I contribute?

We're happy to accept any contribution, big or small. You can find our list of issues [here](https://github.com/WarwickTabletop/tablebot/issues), which details bugs and feature requests. Both code (with documentation) and documentation alone is accepted - we want this project to be as accessible as possible, so any contributions must come with documentation or add documentation.

If you think of a feature you'd like added or find a bug in the current implementation please do create a new ticket! There's no obligation to implement the issue. If you don't have any ideas but do want to get involved with programming you can check the issues page for new features and fixes to work on. If you're not too familiar with Haskell or our codebase, look out for the "good first issue" label. We put this on issues that we think would be good for newcomers to the language/project to get started on.

If you have trouble at any time, please do ask for help in an issue thread or on our Discord. The maintainers generally communicate in the [Tabletop Discord server](https://warwicktabletop.co.uk/discord). The channel we use, #computers-were-a-mistake, is opt-in so you'll need to give yourself the @computer_person role in #roles. Finally, you can also check out the [tutorials](tutorials) in the repository and pre-existing solutions in the code for guidance.

## How to contribute

### The basics

If you'd like to contribute code, these are the steps you'll need to follow along with some tips. (If you've contributed to an open-source Haskell project before, you likely know all of this.)

* You'll need to fork this repo if you haven't already, and then create a branch for the feature you're implementing. Please split up features into different branches where you can.
* When a feature is ready to be merged into the bot, make a pull request from the feature branch to the main repo. In the next section we'll talk about pull requests.
* Before making a pull request (PR), **make sure your branch is up to date with main**. The CI must also not complain, so the code must compile and not throw any formatting errors. We use `ormolu` to check for the latter, which is detailed later.
* If you need help finding a function to do a particular task, you can search on [Hoogle](https://hoogle.haskell.org/). The two libraries that deal with parsing are `Text.Megaparsec` and `Control.Monad.Combinators`, and `Discord` is the package that deals with Discord itself. You can filter by package if that helps to find certain functions.

You can check out the [README](README.md) for a brief overview on how to set up a local bot for testing. If you've never done something like this before, see [SETUP.md](SETUP.md).

### Writing good Pull Requests (PRs)

Writing good PRs is hard. As such, here are some important points to consider when writing your PR to make it easier for the reviewer.

* **Ask yourself: what does this PR contribute?** It is very important that you're clear about all of the features being added, because they justify the changes you have made and point out what exactly needs testing. Mention _everything_ even if it is just cleaning up a file - this makes it clear what the point is of each change and doesn't leave the reviewer guessing why you've added a certain change.
* **Documentation is key.** PRs without documentation will be rejected. A few points to consider about documentation:
    * Functions should have top-level documentation explaining what they do (even if it is very brief) unless they are self-evident. Classes should be justified in the same way. Use Haddock style.
    * In larger implementations, it may help to talk about the high-level structure of your implementation - e.g. you might have a section of your plugin that deals with parsing messages, another which deals with some specific case, another which deals with the general case and so on. Make it clear how these parts interact. Splitting your implementation into multiple files may help here, with a base file that imports each auxiliary file and puts the results within those files together.
    * Haskell code has a habit of being extremely abstract, and talking about it in the abstract does not aid understanding. Give concrete examples of how the abstract is used to justify its existence - instead of solely saying "we have a parser that doesn't look at `t`", back it up with an example like "this is used within a Discord interaction, so doesn't necessarily have an associated message".
* **Remember the plugin writer as well as the end user.** If you've written something that changes how plugins are written, update existing tutorials or add new ones. Make sure that the API you're defining is clear and easy to use, so doesn't put too much burden on someone writing a plugin.

If you follow these steps, it becomes much easier for the reviewer to understand your code and thus feel confident about accepting it. This also allows the reviewer to make more helpful suggestions about the code itself - both allowing them to verify that the code does what you say it does, and that you've implemented it in a helpful way. The review process should help you write better code as well as making Tablebot as a whole better.

### Ormolu

To maintain consistent formatting you must use Ormolu, which can be installed via stack:

`stack install ormolu`

Then you can run it on every file via:

`ormolu --mode inplace $(git ls-files '*.hs')`

You can see full documentation on the [Ormolu repo](https://github.com/tweag/ormolu#usage).

#### Running Ormolu automatically with git

You may also wish to set up Ormolu to run when you stage a file (get ready to commit it) - this can be done using `.gitattributes` and `.gitconfig` as follows.

1. We need to define what runs on commit, which is done via a filter. Add the following to `~/.gitconfig`:

    ```gitconfig
    [filter "haskell-linting"]
        clean = ormolu
        smudge = cat
    ```

    This globally defines a filter that you can call from individual git respositories.

2. Now we need to tell this git repository that it should run this filter. Simply add the following to `.gitattributes`. Note that this has been added to `.gitignore` to avoid people who don't want this filter being forced to use it.

    ```gitattributes
    *.hs filter=haskell-linting
    ```

That's it! (With thanks to Sam Coy for explaining this process)

