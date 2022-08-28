// const sqrt_iter = (guess: number, x: number): number => is_good_enough(guess, x) ? guess : sqrt_iter(improve(guess, x), x)
// const improve = (guess: number, x: number) => average(guess, x / guess)
const abs = (x: number) => x > 0 ? x : x === 0 ? 0 : - x
 const average = (x: number, y: number) => (x + y) / 2
 const is_good_enough = (guess: number, x: number): boolean => abs(improve(guess, x) - guess) < guess * 0.001
// const sqrt = (x: number) => sqrt_iter(1, x)
// console.log(sqrt(9))


const cube_iter = (guess: number, x: number): number => is_good_enough(guess, x) ? guess : cube_iter(improve(guess, x), x)
const improve = (guess: number, x: number) => (((x / guess * guess) + (2 * guess))/ 3)

const cube =(x:number) => cube_iter(1, x)

console.log(cube(16))

