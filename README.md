# Thunderstruct

This is a structured code editor based around "cursor trees".
The structure units it supports happen to include characters, words and lines, but it also supports arbitrary AST nodes (so far only for s-expressions).

## Structure units

- `Char`
- `Word`
- `Line`
- `ASTNode`

## Cursor tree

A "cursor tree" is a list of structure unit types indicies into them. It can be thought of as a query into the file's structure, which, when executed, points to a particular range of characters into the buffer.

Each element in the tree queries into the part of the buffer selected by the remainder of the tree, so it is "reversed" in a sense.
The tree you see at the bottom left of the screen is actually the inverse of the internal structure for ease of use.

For example, the tree [Char 10,Line 15] queries the 11th character in the 16th line.

Some terminology for the pointees of the cursor (actual structure units in the buffer) includes:

- "parent": A unit which contains the pointee;
- "child": A unit which is contained by the pointee;
- "sibling": A unit obtainable by changing the index of the first element in the cursor tree;
- "cousin": A unit obtainable by changing the index of the second element in the cursor tree (i.e. the parent of the pointee);

Changing the tree is done with "meta" commands.
Depress the `Alt` key, and then press `h` and `l` to change the type of the first structure unit in the tree, `j` to remove the first structure unit, and `k` to prepend a structure unit (which one gets added is determined by the unit you're currently on, and the position of the cursor in the buffer).
The current position of the cursor in the buffer is indicated with different background colors: the lightest shade is the unit actually being pointed to, a darker shade marks the "parent" of the unit, and the darkest one marks the "grandparent".
For [Char,Line] cursors, this is analagous to how some editors highlight the current line and characters.

The initial cursor is [], which points to the entire buffer.

## Modes

The first mode you will find yourself in is the `Normal` mode. It is a mode in which you can move around the file and jump to other modes.

Navigation is done with h, j, k, l keys, vim-style: h moves to the previous "sibling", j moves to the next "cousin", k moves to the next

If you press `i` while in `Normal` mode, the cursor will change to make sure that the first unit type is `Char`, and you will get into the `Insert` mode . When you're in this mode, typing will insert characters in the file, at the cursor position.
If your typing happens to change the structure of the file, e.g. interrupt a line or a word, your cursor tree will be adjusted accordingly so that it points directly after the character you've just typed in.

