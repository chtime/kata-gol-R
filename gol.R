# statics ----

ALIVE = 1
EMPTY = 0
BORDER= 0
ALIVE_CHAR = "*"
EMPTY_CHAR = "."


# functions ----

run = function(cols, rows, density=0.3, iterations=200, disp=TRUE) {
  storage = array(NA, c(cols, rows, iterations))
  canvas = populate.binomial(canvas(cols, rows), density)
  #canvas = populate.random(canvas(cols, rows))
  for (iteration in 1:iterations) {
    storage[,,iteration] = canvas
    canvas = population.evolve(canvas)
    if (disp) { 
      display(canvas)
      user.continue = readline(prompt="Enter/Q: ")
      if (user.continue == "Q") {
        break
      }
    }
  }
  return(storage)
}

canvas = function(cols, rows) {
  return(matrix(data=EMPTY, nrow=rows, ncol=cols))
}

display = function(canvas) {
  for (row.idx in 1:nrow(canvas)) {
    for (col.idx in 1:ncol(canvas)) {
      cell = canvas[row.idx, col.idx]
      if (cell == ALIVE) {
        c = ALIVE_CHAR
      } else {
        c = EMPTY_CHAR
      }
      cat(c)
    }
    cat("\n")
  }
  return(canvas)
}

populate.random = function(canvas, popsize=NA) {
  if (is.na(popsize)) {
    popsize = floor(length(canvas) * 0.5)
  }
  
  cells.to.populate = sample.int(n=length(canvas), size=popsize)
  canvas[cells.to.populate] = ALIVE
  return(canvas)
}

populate.binomial = function(canvas, density=.3) {
  for (row.idx in 1:nrow(canvas)) {
    cells.to.populate = rbinom(n=ncol(canvas), size=1, prob=density)
    canvas[row.idx,] = as.vector(cells.to.populate)
  }
  return(canvas)
}

populate = function(canvas, populated.cells) {
  canvas[populated.cells] = ALIVE
  return(canvas)
}

population.alive = function(canvas) {
  return(sum(canvas == ALIVE) > 0)
}

population.evolve = function(canvas) {
  evolution = canvas
  canvas.next = matrix(EMPTY, ncol=ncol(canvas), nrow=nrow(canvas))
  
  # add padding
  evolution = cbind(BORDER, evolution)
  evolution = cbind(evolution, BORDER)
  evolution = rbind(BORDER, evolution)
  evolution = rbind(evolution, BORDER)

  # skip the new border when checking cells
  for (row.idx in 2:(nrow(evolution) - 1 )) {
    for(col.idx in 2:(ncol(evolution) - 1 )) {
      neighbors = evolution[
        # these parentheses are *not* optional
        (row.idx - 1) : (row.idx + 1),
        (col.idx - 1) : (col.idx + 1)
      ]
      neighbors.alive = sum(neighbors == ALIVE)

      underpopulated = neighbors.alive < 2
      overpopulated = neighbors.alive > 3
      viable = !underpopulated && !overpopulated
      expanding = neighbors.alive == 3
      
      focal.cell = evolution[row.idx, col.idx]
      evolved.cell = EMPTY
      
      if (focal.cell == ALIVE) {
        if (viable) {
          evolved.cell = ALIVE
        } else {
          evolved.cell = EMPTY
        } 
      } else if (expanding) {
        evolved.cell = ALIVE
      } 
      
      # ignore the border for target
      canvas.next[row.idx - 1, col.idx - 1] = evolved.cell
    }
  }
  
  return(canvas.next)
}
