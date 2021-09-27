// let nodeArg = Belt.Array.get(Node.Process.argv, 0);
module Bullet {
  module BulletType = {
    type t =
      | Question
      | Todo
      | Item
  };

  type t = {
      bulletType : BulletType.t,
      value : string
  }
}


module BookCompare = Belt.Id.MakeComparable({
  type t = Bullet.t;
  let cmp = (a, b) => compare(a.Bullet.bulletType, b.Bullet.bulletType)
});

type bulletMapType = Belt.Map.t(Bullet.t, list(string), BookCompare.identity);

let bulletOfString = (str: string): Bullet.t => {
    let indicator = Js.String.substring(~from=0, ~to_=2, str);
    let bulletType = 
        switch (indicator) {
        | ".-" => Bullet.Todo
        | ".?" => Bullet.Question
        | ".." => Bullet.Item
        | _ => Bullet.Item
        };
    let value = Js.String.substring(~from=3, ~to_=6, str);
    {bulletType, value}
}

let bulletReducer = (accumulatedBullets: bulletMapType, item: Bullet.t) : 
    list(Bullet.t) => {
  let n = Belt.Map.getWithDefault(accumulatedBullets, item.Bullet., 0); 
  Belt.Map.set(accumulatedMap, item.color, list{})
}

let processBullets = (bullets: list(string)): list(Bullet.value) => {
    Belt.List.map(bullets, bulletOfString);
};

// let bullerReducer = (accumulatedMap: bulletMapType, item: Bullet.t): colorMapType
//  => {
//   let n = Belt.Map.getWithDefault(accumulatedMap, item.color, 0); 
//   Belt.Map.set(accumulatedMap, item.color, n + item.quantity);
// }

let filename = "test.md";
let fileContents = Node.Fs.readFileAsUtf8Sync(filename);
let lines = Js.String.split("\n", fileContents);

let bulletsList = ArrayLabels.to_list(lines)
 |> processBullets;

Belt.List.forEach(bulletsList, (a) => Js.log(a));

// Belt.Map.make(~id=(module BullterComparator))

