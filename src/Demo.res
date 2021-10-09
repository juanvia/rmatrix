let gg = Matrix.makeEmpty()
Js.log(gg)

let sq = Matrix.make(1, 2, [3.0, 4.0])

Js.log({"gg": gg, "sq": sq})

let qs = Matrix.clone(sq)
qs.data[1] = 417.0
Js.log({"qs": qs, "sq": sq})

let tateti = Matrix.make(3, 3, [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0])
let second = Matrix.row(2, tateti)
let third = Matrix.column(1, tateti)
Js.log({"second": second})
Js.log({"third": third})
let tatetito = Matrix.appendColumn (tateti, Matrix.makeColumnVector(3,[2.0,3.0,4.0]))
Js.log({"tatetito":tatetito})
let tatetitotu = Matrix.appendRow (tatetito, Matrix.makeRowVector(4,[5.0,6.0,7.0,8.0]))
Js.log({"tatetitotu":tatetitotu})

