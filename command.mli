type object_phrase = string list

(** The type [command] represents a player command that is decomposed
    into a verb and possibly an object phrase. *)
type command =
  | Go of object_phrase
  | Castle of string
  | Rules
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed of string

(** [parse str] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The rest of the words, if any, become the
    object phrase.

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if the command is malformed. A command is
    {i malformed} if the verb is neither "quit" nor "go", or if the verb
    is "quit" and there is a non-empty object phrase, or if the verb is
    "go" and there is an empty object phrase.*)
val parse : string -> command

(* [str_to_chars] takes a string s and turns it into a list of the
   characters of s *)
val str_to_chars : string -> char list

(* [str_to_pos] takes in a string and turns it into a board position.
   Raises: [Empty] if [str] is the empty string.

   Raises: [Malformed] if the command is malformed. A command is
   malformed if str is not in the correct format (e.g. single character,
   more than 2 characters, invalid position) *)
val str_to_pos : string -> Board.position
