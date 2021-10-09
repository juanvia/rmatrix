open Js.Array2

type t = {
  rows: int,
  cols: int,
  data: array<float>,
}

exception Unmatch(string)
exception RangeError(string)

module Validations = {
  let validateForMake = (rows, cols, data) => {
    if rows * cols != length(data) {
      raise(
        Unmatch(
          "The data array length (" ++
          length(data)->Belt.Int.toString ++
          ") is different of the product (" ++
          (rows * cols)->Belt.Int.toString ++
          ") of rows (" ++
          rows->Belt.Int.toString ++
          ") and cols (" ++
          cols->Belt.Int.toString ++ ")",
        ),
      )
    }
  }
  let validateForRow = (i, rows, _, _) => {
    if i < 0 || i >= rows {
      raise(RangeError(j`Index $i is out of bounds (0-${(rows - 1)->Belt.Int.toString})`))
    }
  }
  let validateForColumn = (j, _, cols, _) => {
    if j < 0 || j >= cols {
      raise(RangeError(j`Index $j is out of bounds (0-${(cols - 1)->Belt.Int.toString})`))
    }
  }
  let validateForAppendRow = (a, row) => {
    if a.cols !== 0 && a.cols !== length(row.data) {
      raise(
        Unmatch(
          j`Cannot append as a row a vector of length ${length(
              row.data,
            )->Belt.Int.toString} to a matrix of ${a.cols->Belt.Int.toString} columns.`,
        ),
      )
    }
  }
  let validateForAppendColumn = (theMatrix, theColumn) => {
    if theMatrix.rows !== 0 && theMatrix.rows !== length(theColumn.data) {
      raise(
        Unmatch(
          j`Cannot append as a column a vector of length ${length(
              theColumn.data,
            )->Belt.Int.toString} to a matrix of ${theMatrix.rows->Belt.Int.toString} rows.`,
        ),
      )
    }
  }
}

let make = (rows, cols, data) => {
  Validations.validateForMake(rows, cols, data)
  {rows: rows, cols: cols, data: data}
}

let makeEmpty = () => make(0, 0, [])
let makeColumnVector = (rows, data) => make(rows, 1, data)
let makeRowVector = (cols, data) => make(1, cols, data)
let makeVector = makeColumnVector
let makeMatrix = (rows, cols, data) => make(rows, cols, data)
let makeEmptyMatrix = makeEmpty

let clone = ({rows, cols, data}: t) => {rows: rows, cols: cols, data: data->copy}

let row = (i, {rows, cols, data}: t) => {
  Validations.validateForRow(i, rows, cols, data)

  let theRowData = []
  for j in 0 to cols - 1 {
    let _ = theRowData->push(data[i * cols + j])
  }

  make(1, cols, theRowData)
}

let column = (j: int, {rows, cols, data}: t) => {
  Validations.validateForColumn(j, rows, cols, data)

  let theColumnData: array<float> = []
  for i in 0 to rows - 1 {
    let _ = theColumnData->push(data[i * cols + j])
  }

  make(rows, 1, theColumnData)
}

let appendRow = (theMatrix: t, theVector: t): t => {
  Validations.validateForAppendRow(theMatrix, theVector)

  make(theMatrix.rows + 1, length(theVector.data), concat(theMatrix.data, theVector.data))
}

let appendColumn = (theMatrix: t, theVector: t): t => {
  Validations.validateForAppendColumn(theMatrix, theVector)

  let newData = []
  for i in 0 to length(theVector.data) - 1 {
    // First, the old elements of the column...
    for j in 0 to theMatrix.cols - 1 {
      let _ = newData->push(theMatrix.data[i * theMatrix.cols + j])
    }
    // ...and then, the newcomer element
    let _ = newData->push(theVector.data[i])
  }
  // now 'newData' contains the vector elements inserted in the right places
  
  make(theMatrix.rows, theMatrix.cols + 1, newData)
}
