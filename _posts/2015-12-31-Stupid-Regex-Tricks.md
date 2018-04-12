---
layout: post
title: Stupid Regex Tricks
comments: True
---


## Regexes, the duct tape of CS

Regexes are truly amazing. If you haven't heard of them, they are a quick way to find (and sometimes replace) some body of text with another body of text. The syntax, while it has its critics, is generally pretty intuitive; for example a zip code might be represented as

```perl
[0-9]{5}(\s*-\s*[0-9]{4})?
```

which is read as "a five digit code optionally followed by a spaced dash and a four digit code". Some whitespace and named groups might help the reading (use `COMMENTS` mode when parsing)

```perl
(?P<main_code>
    [0-9]{5}
)
( # optional extension
    \s*-\s*     # padded dash
    (?P<optional_extension>
        [0-9]{4}
    )
)?
```

<!--end excerpt-->

Note the `?P<names>`, which allow you to refer to those parts of the match in a replace string; for example:

```perl
Your main code is "\g<main_code>" and your optional extension is "\g<optional_extension>"
```

Of course, you can completely abuse regex. Here's one I wrote a few years ago to parse JFugue blocks (I replaced around 1000 lines of code with this!)

```java
String regex = "((?P<par>\\+)?|(?P<seq>_)?)(((?P<notenumber>"
				+ BRACKETED_STRING_REGEX
				+ ")|((?P<notetype>"
				+ NOTE_TYPE_REGEX
				+ ")(?P<octave>"
				+ INTEGER_REGEX
				+ ")?)(?P<chordType>"
				+ CHORDS_REGEX
				+ ")?((\\^(?P<inversionNote>"
				+ NOTE_TYPE_REGEX
				+ "[\\d]*))|(\\^\\[(?P<inversionValue>[\\d]+)\\])|(?P<inversionCount>[\\^]+))?)|(?P<isRest>R))(?P<test>(?P<startTie>-)?((/(?P<numericDuration>("
				+ BRACKETED_STRING_REGEX
				+ ")|("
				+ DECIMAL_REGEX
				+ ")))|((?P<charDuration>[WHQISTXO\\.]+)?(?P<triplet>\\*((?P<tripletnum>[\\d]+):(?P<tripletden>[\\d]+))?)?))(?P<endTie>-)?)(A(?P<attackVelocity>("
				+ BRACKETED_STRING_REGEX + ")|(" + INTEGER_REGEX
				+ ")))?(D(?P<decayVelocity>(" + BRACKETED_STRING_REGEX
				+ ")|(" + INTEGER_REGEX + ")))?";
```

De-java-ing, we get the final regex:

```perl
(
    (?P<par>\+)?
    |
    (?P<seq>_)?
)
(
    (
        (?P<notenumber>
        \[[^\[]+\])
        |
        (
            (?P<notetype>[ABCDEFG][BN#]*)
            (?P<octave>[\d]+)?
        )
        (?P<chordType>DOM7<5<9|DOM7>5>9|DOM7>5<9|DOM7<5>9|MINMAJ7|DOM7<5|DOM7>5|MAJ7<5|MAJ7>5|MIN11|DOM11|DOM13|MIN13|MAJ13|DOM7|MAJ7|MIN7|SUS4|SUS2|MAJ6|MIN6|DOM9|MAJ9|MIN9|DIM7|ADD9|DAVE|MAJ|MIN|AUG|DIM)?
        (
            (\^(?P<inversionNote>[ABCDEFG][BN#]*[\d]*))
            |
            (\^\[(?P<inversionValue>[\d]+)\])
            |
            (?P<inversionCount>[\^]+)
        )?
    )
    |
    (?P<isRest>R)
)
(?P<test>
    (?P<startTie>-)?
    (
        (
            /
            (?P<numericDuration>
                (\[[^\[]+\])
                |
                ([\d\.]+)
            )
        )
        |
        (
            (?P<charDuration>[WHQISTXO\.]+)?
            (?P<triplet>
                \*
                (
                    (?P<tripletnum>[\d]+)
                    :
                    (?P<tripletden>[\d]+)
                )?
            )?
        )
    )
    (?P<endTie>-)?
)
(
    A
    (?P<attackVelocity>
        (\[[^\[]+\])
        |
        ([\d]+)
    )
)?
(
    D
    (?P<decayVelocity>
        (\[[^\[]+\])
        |
        ([\d]+)
    )
)?
```

While I'm pretty `proud|ashamed` of that last one, it has nothing on [the email regex](http://www.ex-parrot.com/pdw/Mail-RFC822-Address.html).

## Repeatedly applied regexes

While regexes are of admittedly low utility beyond simple find and replace, repeatedly applied regexes are much more powerful.

Let's define a stupid regex tricks (SRT) format as follows:

 - A simple replace is in the format `s/regex/replacement/` (syntax from Perl). When executed, it simply replaces its regex with its replacement. If you want an actual `/`, use `//`
 - A compound replace is a series of simple replacements. It is considered a match if any of the statements matches.
 - A repeat statement is the word `repeat` followed an indented compound replace. It continues until its body is not a match
 - Anything following a # is considered a comment if it at the beginning of the file or preceded by a space

Implementing this isn't that complicated; an interpret, along with the code samples below, is provided [here](/resources/2015-12-31).

## Convert Binary to Unary

Let's specify an input format: a string of 1s and 0s, and an output format, a string of 1s.

```perl
repeat
    s/\b([01]+)0\b/\1,\1/
    s/\b([01]+)1\b/\g<1>0,1/
s/,//
```

Note the use of `\1` as shorthand for the first capturing group and that `\g<1>0` is a way of writing `\1` followed by `0` without it being interpreted as `\g<10>`


This function replaces any odd binary number with "that number,1" and any even binary number with "that number/2, that number/2"

## Add one to a binary number

Input/Output, strings of 0s and 1s.

```perl
s/(.+)/\1,/
repeat
    s/([01]*)0,/\g<1>1/
    s/([01]*)1,/\1,0/
    s/^,/1/
```

This particular regex works by using a comma to represent a "carry". It starts the carry off at the end, and then moves backwards in the number. If it finds a 0, it replaces it with a 1 and is done. If it finds a 1, it replaces it with a 0 and moves the carry left. If the carry gets to the front of the number, it appends a 1 to the start of the number.

## Add two binary numbers

OK, so this puts together the last two examples. It takes expressions of the from "100110101 + 1010111" and outputs the binary sum of the two numbers.

```perl
s/([01]+)[^01]+([01]+)/\1,\2/
repeat
    s/\b([01]+)0\b/\1,\1/
    s/\b([01]+)1\b/\g<1>0,1/
s/,//
s/1/;/
repeat
    s/[^;]*;/\0,/
    repeat
        s/([01]*)0,/\g<1>1/
        s/([01]*)1,/\1,0/
        s/^,/1/
```

It converts both of the numbers to unary, then painstakingly counts up each one in binary! Not the most efficient add, but it's done entirely using text substitution.

A more efficient add is left as an exercise; it should not be difficult, just tedious.

## BF interpreter.

BF is a popular esoteric programming language (whose name might break some content filters, so I'll continue to call it BF). It has a single tape of memory (we can think of this as a list of bytes) and seven total commands:

 - `+`: increment the current byte
 - `-`: decrement the current byte
 - `>`: move the current pointer to the right
 - `<`: move the current pointer to the left
 - `.`: print the current byte
 - `,`: read the current byte from input
 - `[` and `]`: a while loop on the value of the current pointer

The interesting thing about BF is that it is completely Turing-complete. That is, it can solve any math problem that any other programming language can solve. Therefore, if we can interpret BF, we can prove that SRT is Turing-complete as well.

At first glance, the last command seems insurmountably difficult. We all know that regular expressions can't parse non-regular grammars, famously [XML](http://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags/1732454#1732454).

However, recursively applied regex can in fact be used for this task. Entering a while loop can be accomplished by setting the initial `[` to a `{` and exiting it can be accomplished by setting the initial `{` back. In this way, we know that the last `{` is the position to jump to when exiting.

To preserve sanity, I am not going to include the 512 line boilerplate on both ends to convert ASCII into an appropriate internal format, I'm going to leave that an exercise. Also, since there is no way for SRT to accept input, a crucial part of BF, it will take two lines as input: a valid BF program with no comments (any characters other than those given above) and the input to the BF program (comma separated binary) and output the result of the BF program.

Initially, I was using unary internally, but that's a complete pain to type and takes forever to process. Also, it's only a 3 line overhead.

Internally, it will store the remaining program on the first line, the remaining input on the second, it's working stack on the third (comma separated unary, with target cell starting with a `;`), and the program output on the fourth.

```perl
# sanitize BF code
repeat
    s/^(.*)[^+\-.,<>\[\]\n]+/\1/
# add empty second line if none exists
s/^([^\n]*(?!\n))$/\1\n/
# make sure that there are at most two lines
s/^.*\n.*\n(\n|.)*$/Error: Input should have at most two lines/
# make sure the second line is comma-separated binary
s/.*\n.*[^01,]/Error: The second line should be in comma separated binary/
# sanitize second line (before @ everything is sanitized)
s/\n(.+)(?<!,)/\n\1,/
s/\n/\n@/
repeat
    s/@$//
    s/(.|\n)*@([01]{8}[01]+),(.|\n)*/Error: bytes must be at most 11111111/
    s/@([01]{8}),/\1,@/
    s/@([01]*),/@0\1,/
# make sure that the number of [ == the number of ]
s/\[/{/
s/\]/}/
repeat
    s/\{(.+)\}/[\1]/
    s/\}(.+)\{/]\1[/
s/^(\n|.)*\{(\n|.)*$/Error: unmatched [/
s/^(\n|.)*\}(\n|.)*$/Error: unmatched ]/
# add lines for data and output. Add cursor @ to front.
s/([^\n]*)\n([^\n]*)\n?/@\1\n\2\n;00000000,\n/
repeat
    # handle + edge case
    s/@\+(.*\n.*\n[^;]*;)11111111(,.*)/+@\g<1>00000000\2/
    # handle + standard case
    #   : represents current position of add
    s/@\+(.*\n.*\n[^;]*;[01]+)/+@\1:/
    repeat
        s/(;[01]*)0:/\g<1>1/
        # carry by moving : to the left
        s/(;[01]*)1:/\g<1>:0/
        # handle overflow by ignoring high bit
        s/;:/;/
    # handle - edge case
    s/@-(.*\n.*\n[^;]*;)00000000(,.*)/-@\g<1>11111111\2/
    # handle - standard case
    s/@-(.*\n.*\n[^;]*;[01]+)(,.*)/-@\g<1>:\2/
    repeat
        s/(;[01]*)1:/\g<1>0/
        s/(;[01]*)0:/\g<1>:1/
        # handle overflow by ignoring high bit
        s/;:/;/
    # output current byte
    s/@\.(.*\n.*\n[^;]*;)([^,]*)(.*\n.*)/.@\1\2\3,\2/
    # error if no input when input expected
    s/@,.*\n[^01,]*\n(\n|.)*/Error: input expected but none found/
    # input current byte
    s/@,(.*\n)([^,]*),?(.*\n[^;]*;)([^,]*),/,@\1\3\2,/
    # shift right (edge case when at right)
    s/@>(.*\n.*\n)([^;]*);([01]*,)\n/>@\1\2\3;00000000,\n/
    # shift right (other cases).
    # The (.+) at the end makes sure this doesn't cover same case as above
    s/@>(.*\n.*\n)([^;]*);([01]*,)(.+\n)/>@\1\2\3;\4/
    # shift left (edge case when at left)
    s/(\n|.)*@<.*\n.*\n;(\n|.)*/Error: Pointer attempted to be moved before 0/
    # shift left in other cases (lazy .*? so that it moves the ; all the way to the left.)
    s/@<(.*\n.*\n.*?)([01]*,);/<@\1;\2/
    # '[' in the skip case; replace with ! and skip through
    s/@\[(.*\n.*\n[^;]*;0+,)/!@\1/
    repeat
        # If current block is ] terminated, skip to end and replace opening ! with [
        s/!([^!]*)@([^[\]!]*)\]/[\1\2]@/
        # If there is a nested [ block, replace with a ! so it will too be processed.
        # move the cursor inside the block
        s/!([^!]*)@([^[\]!]+)\[/!\1\2!@/
    # '[' in any other case; move the cursor to the right, replace current bracket with brace
    s/@\[(.*\n.*\n[^;]*;[01]+,)/{@\1/
    # encountered close bracket. move back to the last open brace
    s/\{([^{]*)@\]/@[\1]/

# get rid of everything but output
s/.*\n.*\n.*\n//
# get rid of leading , in output
s/^,//
```

Try it out on some BF samples, which you can find online. I have tested it, but do not guarantee it's correctness.
