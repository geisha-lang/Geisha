;; Int -> Int
def shit = c ->
  let a = if (c == 8) c + 3 else 8
  in {
    a = a + 1
    a
  }

;; Test operator type infer
;; Int -> Int
def fuck = a -> a + 5

;; Test let-in
;; Void -> Int
def wtd = -> let twoB = 3 in twoB

;; Test let-in with block
;; Void -> Int
def hahah = -> let a = 3 in { a }

;; Int -> Int
def ha1 = c -> let a = c in {
  a = a + 1
  a
}

;; Test if-else with let-in
;; Int -> Int
def ha2 = (c) -> let a = if (true) c + 3 else 8 in a

;; Test if-else
;; Int -> Int
def ha3 = c -> if (c == 10) c + 1 else c

def proA = b -> cons(1, b)

;; Test global define environment
;; ((Int -> Int), Int) -> Int
def main = (omg, holy) ->
  shit(omg(holy + 1))

