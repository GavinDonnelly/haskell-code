Haskell Code
============
A place to store Haskell code snippets, often mathematical or functional proofs, or just programming structures. Before I get into the contents of this repo. I would like to thank the people that taught me most of my functional programming knowlege.

- [Prof Neil Ghani](https://personal.cis.strath.ac.uk/neil.ghani/)
- [Dr Conor McBride](https://personal.cis.strath.ac.uk/conor.mcbride/)

---

## Overview
This Repo is split into directories of code,
- **[BasicCode:](BasicCode/)** Files of basic haskell code, manipuation of strings, integers and lists, some sorting algorithms.
- **[Experimental:](Experimental/)** Small programs built on simple experimental ideas like a bank system or the game of hangman.
- **[Logic and Proofs:](Logic-and-Proofs/)** More structured code, laying out different concepts like propositional logic or topological sorting.

## Usage
Please feel free to use this code and learn from it thats what I wrote it to teach myself through it. Some of the code is inspired by questions I was given or Exam material. So is inspired by a simple idea.

If you have never seen Haskell I recommend visting [Haskell.org](https://www.haskell.org/), they have a great in browser compiler to try it out and lead you through the basics. There is also plenty of documentation to get you set up on your machine if you really want to get serious and try some of your own or my code out.

## Running
Remember, when running Haskell code to learn or explore the programming concepts **[GCHi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html)** is your friend. Running is a program is as easy as:

```bash
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> :load strings-and-lists-v1_4.hs 
	[1 of 1] Compiling Main             ( strings-and-lists-v1_4.hs, interpreted )
	Ok, modules loaded: Main.
*Main> take 3 "abcde"
"abc"
*Main> :q
Leaving GHCi.
$
```