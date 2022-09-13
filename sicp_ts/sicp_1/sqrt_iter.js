// const sqrt_iter = (guess: number, x: number): number => is_good_enough(guess, x) ? guess : sqrt_iter(improve(guess, x), x)
// const improve = (guess: number, x: number) => average(guess, x / guess)
// const abs = (x ) => x > 0 ? x : x === 0 ? 0 : - x
//
//
//silence
// const is_good_enough = (guess:  x: number): boolean => abs(improve(guess, x) - guess) < guess * 0.001
// // const sqrt = (x: number) => sqrt_iter(1, x)
// // console.log(sqrt(9))
//
// const cube_iter = (guess: number, x: number): number => is_good_enough(guess, x) ? guess : cube_iter(improve(guess, x), x)
// const improve = (guess: number, x: number) => (((x / guess * guess) + (2 * guess)) / 3)
//
// const cube = (x: number) => cube_iter(1, x)
//
// console.log(cube(16))
//
//
//
// function square(x: number) {
//   return x * x
// }
//
// function sum_of_squares(x: number, y: number) {
//   return square(x) + square(y)
// }
//
// function f(a: number) {
//   return sum_of_squares(a + 1, a * 2)
// }
//
// console.log(f(5))
//
//
// // Frames as the repository of local state
//
// function make_withdraw(balance: number) {
//   return (amount: number) => {
//     if (balance >= amount) {
//       balance = balance - amount
//       return balance
//     } else {
//       return "Insufficient funds"
//     }
//   }
// }
//
//
// const W1 = make_withdraw(100)
//
// W1(50)
//
//
// function make_withdraw2(initial_amount: number) {
//   return (balance =>
//     (amount: number) => {
//       if (balance >= amount) {
//         balance = balance - amount
//         return balance
//       } else {
//         return "Insufficient funds"
//       }
//     })(initial_amount)
// }
//
// const average = (x: number, y: number) => (x + y) / 2
//
//
// const W2 = make_withdraw2(100)
// console.log(W2(20))
//
// function sqrt(x: number) {
//   console.log(x)
//   function is_good_enough(guess: number) {
//     console.log(x, ": inside is_good_enough")
//     return abs(square(guess) - x) < 0.001
//   }
//   function improve(guess: number) {
//     console.log(x, ": inside improve")
//     return average(guess, x / guess)
//   }
//   function sqrt_iter(guess: number): number {
//     console.log(guess, "inside sqrt iter guess")
//     return is_good_enough(guess)
//       ? guess
//       : sqrt_iter(improve(guess))
//   }
//   return sqrt_iter(1)
// }
//
//
//
// sqrt(4)

function make_account(balance) {
  function withdraw(amount) {
    if (balance >= amount) {
      balance = balance - amount
      return balance
    } else {
      return "Insufficiend funds"
    }
  }
  function deposit(amount) {
    balance = balance + amount
    return balance
  }
  function dispatch(m) {
    return m === "withdraw"
      ? withdraw
      : m === "deposit"
        ? deposit
        : "Unknown request : make_account"
  }
  return dispatch
}

function pair(a, b) {
  return {
    head: a,
    tail: b,
  }
}

function is_string(s) {
  return typeof s === 'string';
}

function is_null(v) {
  return v === null;
}
function is_undefined(v) {
  return v === undefined;
}
function is_boolean(v) {
  return typeof v === 'boolean';
}

function equal(a, b) {
  return a === b;
}

function display(s) {
  console.log(s);
}

function _lst_to_array(lst) {
  const vector = []
  while (!is_null(lst)) {
    vector.push(head(lst))
    lst = tail(lst)
  }
  return vector
}

function length(lst) {
  return _lst_to_array(lst).length;
}

function append(x,y)
{
  return is_null(x)
  ? y
  : pair(head(x), append(tail(x),y))
}

function map(proc, lst) {
  return list(..._lst_to_array(lst).map(proc));
}
function list(...args) {
  function iter(args, result) {
    if (!args || args.length === 0) {
      return result;
    }
    const [, ...rest] = args;
    return pair(args[0], iter(rest, null));
  }
  return iter(args, null);
}

function list_ref(xs, n) {
  return n === 0
    ? head(xs)
    : list_ref(tail(xs), n - 1);
}

function set_head(p, value) {
  p.head = value;
}

function set_tail(p, value) {
  p.tail = value;
}
function is_pair(o) {
  return typeof o === 'object';
}
function head(p) {
  return p.head;
}
function tail(p) {
  return p.tail;
}
// for list we write [1, [[2,3] ,[[4, [5,null]], [6, null]]]]
// equals list(1, [2,3], list(4 5) ,6)
function f(x) {
  function is_even(n) {
    return n === 0
      ? true
      : is_odd(n - 1)
  }
  function is_odd(n) {
    return n === 0
      ? false
      : is_even(n - 1)
  }
  return is_even(x)
}


function last_pair(x) {
  return is_null(tail(x))
    ? x
    : last_pair(tail(x))
}

function append_mutator(x, y) {
  set_tail(last_pair(x), y)
  return x
}

const x = list("a","b")
const y = list("c","d")
const z = append(x,y)

display(length(z))

display(tail(x))

const w = append_mutator(x,y)

display(w)

display(tail(x))
