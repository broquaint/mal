function READ(s: string): string {
    return s
}
function EVAL(s: string): string {
    return s
}
function PRINT(s: string): string {
    return s
}
function rep(s: string): string {
    return PRINT(EVAL(READ(s)))
}

let line = prompt('user>')
while (line !== null) {
  console.log(rep(line));
  line = prompt('user>')
}