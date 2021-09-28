module Bullet {
  type bulletType =
    | Question
    | Todo
    | Item

  type t = {
      bulletType : bulletType,
      value : string
  }

  let stringOfBullteType = (bt: bulletType):string => {
      switch (bt) {
        | Question => "Question"
        | Todo => "Todo"
        | Item => "Item"
      };
  }
};

module BulletCompare = Belt.Id.MakeComparable({
  type t = Bullet.bulletType;
  let cmp = (a, b) => compare(a, b)
});

type bulletMapType = Belt.Map.t(Bullet.bulletType, string, BulletCompare.identity);

let bulletOfString = (str: string): option(Bullet.t) => {
  open Bullet
  let value = Js.String.substr(~from=3, str);
  let indicator = Js.String.substring(~from=0, ~to_=2, str);
  switch (indicator) {
    | ".>" => Some({bulletType: Todo, value})
    | ".?" => Some({bulletType: Question, value})
    | ".-" => Some({bulletType: Item, value})
    | _ => None
  };
};

let bulletReducer = (accumulatedMap: bulletMapType, item: Bullet.t): bulletMapType => {
  let n = Belt.Map.getWithDefault(accumulatedMap, item.bulletType, ""); 
  Belt.Map.set(accumulatedMap, item.bulletType, n ++ "* " ++ item.value ++ "\n");
};

let notesReducer = (accumulatedString: string, cmp: Bullet.bulletType, item: string): string => {
  accumulatedString ++ "## " ++ Bullet.stringOfBullteType(cmp) ++ "\n\n" ++ item ++ "\n"
};


let processNotes = (inNotes: string, outNotes: string): unit => {
  let fileContents = Node.Fs.readFileAsUtf8Sync(inNotes);
  let lines = Js.String.split("\n", fileContents);
  let notes = Belt.Array.map(lines, bulletOfString)
    |> Belt.Array.keepMap(_, (bullet)=>bullet)
    |> Belt.List.fromArray(_)
    |> Belt.List.reduce(_, Belt.Map.make(~id=(module BulletCompare)), bulletReducer)
    |> Belt.Map.reduce(_, "", notesReducer);

  let _ = Node.Fs.writeFileAsUtf8Sync(outNotes, "# Notes\n\n" ++ notes);
  ()
};


let nodeArg = Belt.Array.get(Node.Process.argv, 0);
let progArg = Belt.Array.get(Node.Process.argv, 1);
let inFileArg = Belt.Array.get(Node.Process.argv, 2);
let outFileArg = Belt.Array.get(Node.Process.argv, 3);

switch (nodeArg, progArg, inFileArg, outFileArg) {
  | (_, _, Some(inFileName), Some(outFileName)) => processNotes(inFileName, outFileName)
  | (Some(node), Some(prog), _, _) =>
    Js.log("Usage: " ++ node ++ " " ++ prog ++ " inputNote.md" ++ " outputNote.md")
  | (_, _, _, _) =>
    Js.log("Error")
};