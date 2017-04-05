def shit = c ->
  let a = if (c == 8) c + 3 else 8
  in {
    a = a + 1
    a
  }

;; Test operator type infer
def fuck = a -> a + 5

;; Test let-in
def wtd = () -> let twoB = 3 in twoB

;; Test let-in with block
def hahah = () -> let a = 3 in { a }

def ha1 = c -> let a = c in {
  a = a + 1
  a
}

;; Test if-else with let-in
def ha2 = c -> let a = if (c == 8) c + 3 else 8 in a

;; Test if-else
def ha3 = c -> if (c == 10) c + 1 else c

;; Test global define environment
def main = (omg, holy) ->
  shit(omg(holy + 1))

