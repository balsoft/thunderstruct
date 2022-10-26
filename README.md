# Thunderstruct

WIP :warning:

This is a structured code editor based around "cursor trees".
The structure units it supports happen to include characters, words and lines, but it also supports arbitrary AST nodes (so far only for s-expressions).

## Structure units

- `Char`
- `Word`
- `Sentence`
- `Line`
- `Paragraph`
- `ASTNode`

## Cursor tree

A "cursor tree" is a list of structure unit types indicies into them. It can be thought of as a query into the file's structure, which, when executed, points to a particular range of characters into the buffer.

Each element in the tree queries into the part of the buffer selected by the remainder of the tree, so it is "reversed" in a sense.
The tree you see at the bottom left of the screen is actually the inverse of the internal structure for ease of use. We will use the displayed form (which, again, is the reverse of the internal one) from now on in this document.

For example, the tree [Line 15, Char 10] queries the 11th character in the 16th line.

Some terminology for the pointees of the cursor (actual structure units in the buffer) includes:

- "parent": A unit which contains the pointee;
- "child": A unit which is contained by the pointee;
- "sibling": A unit obtainable by changing the index of the first element in the cursor tree;
- "cousin": A unit obtainable by changing the index of the second element in the cursor tree (i.e. the parent of the pointee);

The current position of the cursor in the buffer is indicated with different background colors: the lightest shade is the unit actually being pointed to, a darker shade marks the "parent" of the unit, and the darkest one marks the "grandparent".
For [Line, Char] cursors, this is analagous to how some editors highlight the current line and characters.

The initial cursor is [], which points to the entire buffer.

## Modes

- `Normal`: the default mode. You can navigate around the file (see [Navigation](#navigation)) and jump to other modes. Press ESC from any other mode to get back to `Normal`.
- `Insert`: the characters you type get inserted into the buffer right before the cursor position, and the cursor advances one position. The cursor is orange in this mode.
  If your typing happens to change the structure of the file, e.g. interrupt a line or a word, your cursor tree will be adjusted accordingly so that it points directly after the character you've just typed in.
  Pressing backspace will delete the character right before the cursor.
- `Command`: allows you to type in a command. See [Commands](#commands) for more info.

## Navigation

Navigation is done with h, j, k, l keys, vim-style: h moves to the previous "sibling", j moves to the next "cousin", k moves to the previous "cousin", and l moves to the next "sibling". You can also use the arrow keys (←↓↑→).

Additionally, H, J, K, and L (Shift+h, Shift+j, Shift+k and Shift+l) move to the first "sibling", last "cousin", first "cousin" and last "sibling" respectively. You can also use Home/End to jump to first/last sibling.

If your cursor points to a non-existant object, it will become a 0-length cursor right after the parent of the object.

You can also change the cursor tree itself in a similar manner, jumping around the very structure of the file you're editing. This is the defining feature of thunderstruct.
Depress the `Alt` key, and then press `h` and `l` to change the type of the first structure unit in the tree, `j` to remove the first structure unit (going to the parent of the pointee),
and `k` to prepend a structure unit (the first most abstract child that the pointee has).

You can imagine this as though moving through a cursor tree, laid out something like this:

```
Buffer
+------+-----+---+-------+---------------------+---------------------------+
|      |     |           |                     |                           |
Char   Word  Sentence    Line                  Paragraph                   ASTNode (if buffer parses)
       |     +-----+     +-----+-----+         +-----+-----+---------+     +-----+-----+---------+----------+
       |     |     |     |     |     |         |     |     |         |     |     |     |         |          |
       Char  Char  Word  Char  Word  Sentence  Char  Word  Sentence  Line  Char  Word  Sentence  Paragraph  ASTNode (if parent has it)
                   |           |     |               |     |         |           |     |         |          |
                   Char        Char  …               Char  …         …           Char  …         …          …
```

Then, the "meta-movements" are somewhat similar to actual movements around the file, with h/l moving to siblings.
The difference is that j/k moves to children/parents instead of cousins, but the tree above should give you an intuition if you're familiar with vim-style navigation.

This is probably still confusing, so here are some examples. Suppose you have just started thunderstruct, and so your cursor is `[]`.
If you are in a file which does not parse as an AST (such as this one), pressing Alt+j will bring the cursor to a `[Paragraph 0]`, thus pointing to the first paragraph (and highlighting it).

Pressing Alt+h will bring the cursor to `[Line 0]`, pointing to the first line.

Pressing Alt+j will bring you to a `[Line 0, Word 0]` cursor, which selects the first word of the first line.

Pressing Alt+h will bring you to `[Line 0, Char 0]`, which simulates the way most editors function. Try navigating around the file with h/j/k/l or the arrow keys to make sure it makes sense.

Now, press Alt+l to get back to `[Line, Word]`. Try moving around the file now, and observe how you can easily jump between words in lines.

Now press Alt+k to get back to `[Line]`. Again, try moving around the file. Notice how j and k (and ↓↑) don't do anything, because there are no parents, and instead you change lines with h/l (or ←→).

Press Alt+h to go to `[Word]`, and move around the file that way. Press Alt+h to go to `[Char]`. Press Alt+k to go back to `[]`.

As an excercise, try reaching `[Line,Word,Char]` and `[Paragraph, Word]` yourself.

If you are in a file which does parse as AST (only s-expressions for now), pressing Alt+j will give you [ASTNode 0]. It will select the first parenthesis in the file.
Pressing Alt+j once more will give you [ASTNode 0, ASTNode 0] (if the first parenthesis have at least one element). Get a feel for jumping through the structure.

As an excercise, try reaching `[ASTNode 0, ASTNode 3, ASTNode 1, Char 1]` in this file:

```
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1))) ) )
```

You can undo and redo changes to the cursor. Once again, depress `Alt` and press `u` to undo your cursor change or `Shift+u` (`U`) to redo the change you just undid.

There is also another way to navigate the cursor tree. See `#` and `@` commands in the [Commands](#commands) section.

## Editing

While in `Normal` mode, you can press `d` to delete the pointee, `D` (Shift+d) to delete everything from the beginning of pointee to the beginning of the next sibling, and `Alt+d` to delete the parent of the pointee.

Pressing `y` will yank (copy) the current pointee and insert it to the top of the clipboard, `Y` will yank all characters from beginning of current pointee to the beginning of the next one,
`p` will paste from the top of the clipboard (dropping your cursor to Char if needed), `P` will do the same but remove (pop) the top of the clipboard, leaving older entries,
and `Alt+p` will just remove the top of the clipboard.

You can also press `i`/`I` to go to the first/last Char of the current pointee and enter the `Insert` mode.
Pressing `c` will delete the pointee and go to `Insert` mode in its place, and pressing `C` (Shift+c) will delete everything between the beginning of the pointee to the beginning of the next sibling and then go to `Insert` mode,
and `Alt+c` will delete the parent and then go to `Insert` mode.

You can undo the changes done by `d`, `D`, `Alt+d`, `c`, `C`, `Alt+c`, or things you've typed in a single Insert mode session by pressing `u`. `U` will re-do the change you just undid.

## Commands

To enter Command mode, press `:` while in Normal mode. Now, everything you type will get appended to the current command. `RET` (Enter) executes the command.

So, for example, if you're in Normal mode and press `:` `w` `RET`, a command `"w"` will get executed.

The `"w"` command, coincidentally, is writing. If a file is currently open (e.g. you've started thunderstruct with an argument), it will write to that file.
You can also pass it a file to write by appending characters to the command, e.g. `"wREADME.md"` will write to the file "README.md".

The `"q"` command will quit the editor. If the contents of the buffer differs from the contents of the file on disk, it will tell you so. You can use `"q!"` command to exit anyways.

The `"e"` command will open a file for editing. It can be passed an argument similarly to `"w"`, e.g. `"eREADME.md"` will open README.md.

The `"#"` command will change the cursor tree types to its argument. The argument is comma-separated list of first characters of names of the structure units ('c' for Char, 'w' for Word, etc.). E.g. "#l,c" will change the cursor to be [Line, Character].
The command makes sure to keep the pointee either the same or as close as possible to the current one.

The `"@"` command will either change the cursor tree types or cursor tree indicies, depending on the argument.

- If the argument is same as for `"#"` (comma-separated list of characters), then the end of the cursor tree is updated to have these types.
  E.g. `"@w"` when the cursor is `[Line, Char]` will turn the cursor into `[Line, Word]`, and `"@w,c"` when the cursor is `[Line, Sentence, Word]` will turn the cursor into `[Line, Word, Char]`.
- If the argument is a comma-separted list of non-negative integers, then the end of the cursor tree will be updated to have these indicies.
  E.g. `"@0"` when the cursor is `[Line, Char]` will jump to the first character of the line, and `"@5,9"` will jump to the 10th character of the 6th line.

You can also press `#` or `@` from the Normal mode to get into Command mode with corresponding commands already entered.
