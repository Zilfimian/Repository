# ============================================================
# Chapter 5: Lists and Data Frames# (store multiple types of values at once)
# Notes:
#   - This script uses base R only.
# ============================================================

# V, M, A can store only one type of data.
# ------------------------------------------------------------
# 5.1 Lists
# ------------------------------------------------------------
cat("\n--- 5.1.1 Definition and Component Access ---\n")

# Creating a list
foo <- list(
  matrix(data = 1:4, nrow = 2, ncol = 2),
  c(TRUE, FALSE, TRUE, TRUE),
  "hello"
)

cat("\nfoo (original):\n")
print(foo)

# Number of components
cat("\nlength(foo): ")
print(length(x = foo))

# Member reference with [[ ]]
cat("\nfoo[[1]]:\n")
print(foo[[1]])

cat("\nfoo[[3]]:\n")
print(foo[[3]])

# Treat retrieved component as a normal object
cat("\nfoo[[1]] + 5.5:\n")
print(foo[[1]] + 5.5)

cat("\nfoo[[1]][1,2]: ")
print(foo[[1]][1, 2])

cat("\nfoo[[1]][2, ]:\n")
print(foo[[1]][2, ])

cat("\ncat(foo[[3]], 'you!') -> ")
cat(foo[[3]], "you!\n")

# Overwrite a member using assignment
cat("\nOverwriting foo[[3]] using paste():\n")
print(foo[[3]])
foo[[3]] <- paste(foo[[3]], "you!")
print(foo)

# Attempt to access multiple components with [[c(2,3)]] (does NOT slice)
cat("\nfoo[[c(2,3)]] (demonstrates why [[ ]] is for one member):\n")
print(foo[[c(2, 3)]])

# List slicing with [ ] to select multiple members
cat("\nList slicing: bar <- foo[c(2,3)]\n")
bar <- foo[c(2, 3)]
print(bar)


# ------------------------------------------------------------
# 5.1.2 Naming
# ------------------------------------------------------------
cat("\n--- 5.1.2 Naming ---\n")

names(foo) <- c("mymatrix", "mylogicals", "mystring")
cat("\nfoo after naming:\n")
print(foo)

cat("\nfoo$mymatrix:\n")
print(foo$mymatrix)

cat("\nfoo[[1]] (still works with numeric index):\n")
print(foo[[1]])

cat("\nCheck identical extraction of second column: all(...)\n")
print(all(foo$mymatrix[, 2] == foo[[1]][, 2]))

# Naming components at creation time
cat("\nCreate named list baz using components of foo:\n")
baz <- list(
  tom   = c(foo[[2]], TRUE, TRUE, TRUE, FALSE),
  dick  = "g'day mate",
  harry = foo$mymatrix * 2
)
print(baz)

cat("\nnames(baz): ")
print(names(baz))


# ------------------------------------------------------------
# 5.1.3 Nesting
# ------------------------------------------------------------
cat("\n--- 5.1.3 Nesting ---\n")

# Add a new component to an existing list
baz$bobby <- foo
cat("\nbaz after adding nested list bobby <- foo:\n")
print(baz)

# Extract nested items (different valid ways)  dollar operator 
cat("\nbaz$bobby$mylogicals[1:3]:\n")
print(baz$bobby$mylogicals[1:3])

cat("\nbaz[[4]][[2]][1:3]:\n")
print(baz[[4]][[2]][1:3])

cat("\nbaz[[4]]$mylogicals[1:3]:\n")
print(baz[[4]]$mylogicals[1:3])


# ------------------------------------------------------------
# 5.2 Data Frames
# ------------------------------------------------------------
cat("\n=== 5.2 Data Frames ===\n")

cat("\n--- 5.2.1 Construction ---\n")

mydata <- data.frame(
  person = c("Peter", "Lois", "Meg", "Chris", "Stewie"),
  age    = c(42, 40, 17, 14, 1),
  sex    = factor(c("M", "F", "F", "M", "M"))
)

cat("\nmydata:\n")
print(mydata)

cat("\nmydata[2,2] (Lois's age): ")
print(mydata[2, 2])

cat("\nmydata[3:5,3] (sex for Meg, Chris, Stewie):\n")
print(mydata[3:5, 3])

cat("\nmydata[,c(3,1)] (sex then person):\n")
print(mydata[, c(3, 1)])

cat("\nmydata$age:\n")
print(mydata$age)

cat("\nmydata$age[2] (same as mydata[2,2]): ")
print(mydata$age[2])

cat("\nSize of mydata: nrow, ncol, dim\n")
print(nrow(mydata))
print(ncol(mydata))
print(dim(mydata))

# stringsAsFactors (book note)
cat("\nDemonstrate stringsAsFactors=FALSE to keep person as character:\n")
mydata_chr <- data.frame(
  person = c("Peter", "Lois", "Meg", "Chris", "Stewie"),
  age    = c(42, 40, 17, 14, 1),
  sex    = factor(c("M", "F", "F", "M", "M")),
  stringsAsFactors = TRUE
)

cat("\nmydata_chr$person (should be character):\n")
print(mydata_chr$person)


# ------------------------------------------------------------
# 5.2.2 Adding Data Columns and Combining Data Frames
# ------------------------------------------------------------
cat("\n--- 5.2.2 Adding Columns/Rows and Combining ---\n")

# Add a new record
newrecord <- data.frame(
  person = "Brian",
  age    = 7,
  sex    = factor("M", levels = levels(mydata$sex))
)

cat("\nnewrecord:\n")
print(newrecord)

mydata <- rbind(mydata, newrecord)
cat("\nmydata after rbind(mydata, newrecord):\n")
print(mydata)

# Add a new variable funny
funny <- c("High", "High", "Low", "Med", "High", "Med")
funny <- factor(x = funny, levels = c("Low", "Med", "High"))

cat("\nfunny factor:\n")
print(funny)

mydata <- cbind(mydata, funny)
cat("\nmydata after cbind(mydata, funny):\n")
print(mydata)

# Add age in months using $ operator
mydata$age.mon <- mydata$age * 12
cat("\nmydata after adding age.mon:\n")
print(mydata)


# ------------------------------------------------------------
# 5.2.3 Logical Record Subsets
# ------------------------------------------------------------
cat("\n--- 5.2.3 Logical Record Subsets ---\n")

cat("\nmydata$sex == 'M':\n")
print(mydata$sex == "M")

cat("\nMale-only subset mydata[mydata$sex=='M', ]:\n")
print(mydata[mydata$sex == "M", ])

cat("\nMale-only subset without sex column (column -3):\n")
print(mydata[mydata$sex == "M", -3])

cat("\nMale-only subset selecting columns by name:\n")
print(mydata[mydata$sex == "M", c("person", "age", "funny", "age.mon")])

cat("\nAge>10 OR funny==High:\n")
print(mydata[mydata$age > 10 | mydata$funny == "High", ])

cat("\nEmpty subset example (age > 45):\n")
res_empty <- mydata[mydata$age > 45, ]
print(res_empty)
cat("nrow(empty subset): ")
print(nrow(res_empty))


# ============================================================
# Task
# ============================================================

#  1: [[ ]] vs [ ] (object vs sub-list)
cat("\nTrick 1: [[ ]] vs [ ]\n")
print(foo[[2]])
print(foo[2])
cat("is.list(foo[2]) -> ")
print(is.list(foo[2]))
cat("is.list(foo[[2]]) -> ")
print(is.list(foo[[2]]))

#  2: drop=FALSE to keep data frame output when selecting one column
cat("mydata[, 'age'] simplifies to a vector; drop=FALSE keeps a data frame.\n")
print(mydata[, "age"])
print(mydata[, "age", drop = FALSE])

#  3: Safe factor handling when adding rows (avoid NA due to missing levels)
cat("Always align factor levels when creating new rows.\n")
newrecord_safe <- data.frame(
  person  = "Someone",
  age     = 10,
  sex     = factor("M", levels = levels(mydata$sex)),
  funny   = factor("Med", levels = levels(mydata$funny)),
  age.mon = 120
)

# To rbind, columns must match; here we demonstrate safely matching factor levels.
mydata_safe <- rbind(mydata, newrecord_safe)
cat("\nmydata_safe (with aligned factor levels):\n")
print(tail(mydata_safe, 2))

# 5: Starts-with filtering: substr and grepl
starts_with_S_substr <- mydata[substr(mydata$person, 1, 1) == "S", ]
starts_with_S_grepl  <- mydata[grepl("^S", mydata$person), ]
cat("Using substr:\n")
print(starts_with_S_substr)
cat("Using grepl (regex):\n")
print(starts_with_S_grepl)


# ============================================================
# Exercise 5.1
# ============================================================
# 5.1(a) Create the list with specified components
seq20 <- seq(from = -4, to = 4, length.out = 20)

log_mat <- matrix(
  data = c(FALSE, TRUE, TRUE,
           TRUE, FALSE, TRUE,
           TRUE, FALSE, FALSE),
  nrow = 3, ncol = 3, byrow = FALSE
)

chars <- c("don", "quixote")

facs <- factor(c("LOW", "MED", "LOW", "MED", "MED", "HIGH"),
               levels = c("LOW", "MED", "HIGH"))

lstA <- list(seq20, log_mat, chars, facs)
cat("\nlstA created:\n")
print(lstA)

# (i) Extract row elements 2 and 1 of columns 2 and 3, in that order
cat("\n5.1(a)(i): rows (2,1) of cols (2,3) from logical matrix:\n")
print(lstA[[2]][c(2, 1), c(2, 3)])

# (ii) Overwrite "quixote" -> "Quixote" and "don" -> "Don" inside list; print exact statement
cat("\n5.1(a)(ii): overwrite strings using sub and print statement:\n")
lstA[[3]] <- sub("^don$", "Don", lstA[[3]])
lstA[[3]] <- sub("^quixote$", "Quixote", lstA[[3]])

# Print exactly:
# "Windmills! ATTACK!"-\Don Quixote/
cat('"Windmills! ATTACK!"-\\', paste(lstA[[3]], collapse = " "), '/\n', sep = "")

# (iii) Values from the sequence > 1
cat("\n5.1(a)(iii): values > 1 in seq(-4,4,length=20):\n")
print(lstA[[1]][lstA[[1]] > 1])

# (iv) Indexes assigned the "MED" level
cat("\n5.1(a)(iv): which indexes are MED in factor vector:\n")
print(which(lstA[[4]] == "MED"))


# 5.1(b) Create new list with facs, nums, and nested oldlist
lstB <- list(
  facs    = lstA[[4]],
  nums    = c(3, 2.1, 3.3, 4, 1.5, 4.9),
  oldlist = lstA[c(1, 2, 3)]
)

cat("\nlstB created:\n")
print(lstB)

# (i) Extract elements of facs where nums >= 3
cat("\n5.1(b)(i): facs where nums >= 3:\n")
print(lstB$facs[lstB$nums >= 3])

# (ii) Add flags: length 6 = twofold repetition of 3rd column of logical matrix in oldlist
cat("\n5.1(b)(ii): create flags by repeating 3rd column twice:\n")
col3 <- lstB$oldlist[[2]][, 3]
lstB$flags <- rep(col3, times = 2)
print(lstB$flags)

# (iii) Use flags and ! to extract nums corresponding to FALSE
cat("\n5.1(b)(iii): nums where flags are FALSE (using !flags):\n")
print(lstB$nums[!lstB$flags])

# (iv) Overwrite character vector component of oldlist with "Don Quixote"
cat("\n5.1(b)(iv): overwrite oldlist character vector with single string:\n")
lstB$oldlist[[3]] <- "Don Quixote"
print(lstB$oldlist)

# ============================================================
# Chapter 6: Special Values, Coercion
# ============================================================
# ------------------------------------------------------------
# 6.1 Some Special Values
# ------------------------------------------------------------

# -------------------------
# 6.1.1 Infinity
# -------------------------

foo_inf <- Inf
cat("foo_inf:\n")
print(foo_inf)

bar_inf <- c(3401, Inf, 3.1, -555, Inf, 43)
cat("\nbar_inf:\n")
mean(bar_inf)

baz_inf <- 90000^100
cat("\nbaz_inf (90000^100):\n")
print(baz_inf)
1.797693e+308
.Machine$double.xmax
exp(2000)
factorial(2000)
(1e200) * (1e200)
.Machine$double.xmin


qux_inf <- c(-42, 565, -Inf, -Inf, Inf, -45632.3)
cat("\nqux_inf:\n")
print(qux_inf)

cat("\nBasic operations with Inf/-Inf:\n")
print(Inf * -9)
print(Inf + 1)
print(4 * -Inf)
print(-45.2 - Inf)
print(Inf - 45.2)
print(Inf + Inf)
print(Inf / 23)

cat("\nDivision with infinity and zero:\n")
print(-59 / Inf)
print(-59 / -Inf)
print(-59 / 0)

print(59 / 0)
print(Inf / 0)

cat("\nDetecting infinite and finite values:\n")
print(is.infinite(x = qux_inf)) # do not distinguish between neg-pos
print(is.finite(x = qux_inf))

cat("\nRelational checks:\n")
print(-Inf < Inf)
print(Inf > Inf)
print(qux_inf == Inf)
print(qux_inf == -Inf)


# -------------------------
# 6.1.2 NaN
# -------------------------
# not mathematically defined, undefined for real numbers, or not representable
foo_nan <- NaN
cat("foo_nan:\n")
print(foo_nan)

bar_nan <- c(NaN, 54.3, -2, NaN, 90094.123, -Inf, 55)
cat("\nbar_nan:\n")
print(bar_nan)

cat("\nHow NaN can arise:\n")
print(-Inf + Inf)
print(Inf / Inf)
print(0 / 0)
Inf %% Inf
log(-1)
sqrt(-4)
factorial(-1)
0^(-1)

cat("\nAny operation involving NaN stays NaN:\n")
print(NaN + 1)
print(2 + 6 * (4 - 4) / 0)
print(3.5 ^ (-Inf / Inf))

cat("\nDetect NaN and combine with Inf detection:\n")
print(is.nan(x = bar_nan))
print(!is.nan(x = bar_nan))
print(is.nan(x = bar_nan) | is.infinite(x = bar_nan))

cat("\nRemove NaN or Inf using which + negative indexes:\n")
idx_bad <- which(is.nan(x = bar_nan) | is.infinite(x = bar_nan))
print(idx_bad)
print(bar_nan[-idx_bad])
#NaN==NaNv# relational operators 


# -------------------------
# 6.1.3 NA
# -------------------------
foo_na <- c("character", "a", NA, "with", "string", NA)
cat("foo_na:\n")
print(foo_na)

bar_na <- factor(c("blue", NA, NA, "blue", "green", "blue", NA, "red", "red", NA, "green"))
cat("\nbar_na (factor with NA):\n")
print(bar_na)

baz_na <- matrix(c(1:3, NA, 5, 6, NA, 8, NA), nrow = 3, ncol = 3)
cat("\nbaz_na (matrix with NA):\n")
print(baz_na)

qux_mix <- c(NA, 5.89, Inf, NA, 9.43, -2.35, NaN, 2.10, -8.53, -7.58, NA, -4.58, 2.01, NaN)
cat("\nqux_mix (NA/NaN/Inf mix):\n")
print(qux_mix)

cat("\nis.na flags both NA and NaN (for numeric vectors):\n")
print(is.na(x = qux_mix))

cat("\nIdentify NaN only:\n")
print(which(x = is.nan(x = qux_mix)))

cat("\nIdentify NA only (exclude NaN):\n")
print(which(x = (is.na(x = qux_mix) & !is.nan(x = qux_mix))))

cat("\nRemove NA and NaN using na.omit():\n")
quux_omit <- na.omit(object = qux_mix) #na.action
print(quux_omit)

cat("\nArithmetic with NA yields NA; comparisons with NA/NaN yield NA:\n")
print(3 + 2.1 * NA - 4)
print(3 * c(1, 2, NA, NA, NaN, 6))
print(NA > 76)
print(76 > NaN)
as.numeric(c("L", "Z"))


# -------------------------
# 6.1.4 NULL
# -------------------------
foo_null <- NULL
cat("foo_null:\n")
print(foo_null) # no position to access

bar_single_na <- NA
cat("\nbar_single_na:\n")
print(bar_single_na)

cat("\nCompare c(..., NA, ...) vs c(..., NULL, ...):\n")
print(c(2, 4, NA, 8))
print(c(2, 4, NULL, 8))

cat("\nCompare multiple NA vs multiple NULL:\n")
print(c(NA, NA, NA))
print(c(NULL, NULL, NULL))

cat("\nNULL in arithmetic/relational produces empty vectors of a type:\n")
print(NULL + 53)
print(53 <= NULL)
print(NaN - NULL + NA / Inf)

# NULL in lists/data frames: missing member/column
cat("\nNULL returned when accessing a missing list member:\n")
foo_list <- list(member1 = c(33, 1, 5.2, 7), member2 = "NA or NULL?")
print(foo_list)
print(foo_list$member3)  # doesn't exist -> NULL

cat("\nFill a missing member (member3) after seeing NULL:\n")
foo_list$member3 <- NA
print(foo_list)

# no object at all
x <- list(a = 1, b = 2)
x$c 

x$b <- NULL   # removes column b

#an empty or uninitialized object
# mainly used in fucntion's optional arguments and predefinitions

# ============================================================
# Task 
# ============================================================

# A: A robust "bad value" detector for numeric vectors
# (NA, NaN, Inf, -Inf)
cat("\nTrick A: Identify any 'bad' numeric values (NA/NaN/Inf)\n")
mark_bad <- function(x) {
  is.na(x) | is.infinite(x)
}

x_demo <- c(1, NA, 2, NaN, 3, Inf, -Inf, 4)
print(x_demo)
print(mark_bad(x_demo))
cat("Cleaned (remove bad):\n")
print(x_demo[!mark_bad(x_demo)])

#B: Why you shouldn't use == NA (it's invalid / not meaningful)
cat("\nTrick B: NA comparisons yield NA, so always use is.na\n")
val <- c(NA, 1, 2)
print(val)
cat("val == NA gives:\n")
print(val == NA)  # always NA
cat("Use is.na(val):\n")
print(is.na(val))

# D: Optional argument checks — NULL vs NA
cat("\nTrick D: Optional argument checks — is.null is single TRUE/FALSE\n")
opt.arg <- c("string1", "string2", "string3")
print(is.na(opt.arg))
print(is.null(opt.arg))
opt.arg <- NULL
print(is.null(opt.arg))



# ============================================================
# Chapter 9: Calling Functions — Code Pack
# ============================================================
# ------------------------------------------------------------
# 9.1 Scoping
# ------------------------------------------------------------

# -------------------------
# 9.1.1 Environments
# -------------------------
cat("\n--- 9.1.1 Environments ---\n")

# Global environment: user-created objects live here
foo <- 4 + 5
bar <- "stringtastic"
cat("\nObjects in .GlobalEnv (ls()):\n")
print(ls())

# Package environments: list visible objects in a package
cat("\nFirst few objects in package:graphics (ls('package:graphics')):\n")
print(head(ls("package:graphics"), 20))

# Local (lexical) environment: created when a function is called
# Example: matrix() uses argument name 'data' internally, but R resolves it in the local env
cat("\nCreate a matrix using argument name data (local env inside matrix call):\n")
youthspeak <- matrix(data = c("OMG", "LOL", "WTF", "YOLO"), nrow = 2, ncol = 2)
print(youthspeak)


# -------------------------
# 9.1.2 Search Path
# -------------------------
cat("\n--- 9.1.2 Search Path ---\n")

cat("\nCurrent search() path:\n")
print(search())

# Demonstrate how R finds functions along the search path
baz <- seq(from = 0, to = 3, length.out = 5)
cat("\nSequence baz from seq():\n")
print(baz)

# Identify owning environments (namespaces) of functions
cat("\nOwning environments for selected functions:\n")
print(environment(seq))
print(environment(arrows))

# "cannot find" errors (wrapped in try so the script continues)
cat("\nDemonstrate cannot find errors (handled with try):\n")
try(neither.here(), silent = TRUE)
try(print(nor.there), silent = TRUE)


# -------------------------
# 9.1.3 Reserved and Protected Names
# -------------------------
cat("\n--- 9.1.3 Reserved and Protected Names ---\n")

# Trying to overwrite reserved names causes errors
cat("\nAttempt to assign to NaN (should error):\n")
try(NaN <- 5, silent = TRUE)

# Case sensitivity: these are allowed but confusing
False <- "confusing"
nan <- "this is"
cat("\nCase-variant names are allowed but confusing:\n")
cat(nan, False, "\n")

# T and F are NOT reserved (avoid overriding!)
T <- 42
F <- TRUE
cat("\nOverriding T/F is legal but confusing; F && TRUE now means TRUE && TRUE:\n")
print(F && TRUE)

# Clean global environment (as in book)
cat("\nCleaning global environment with rm(list=ls()):\n")
print(ls())
rm(list = ls())
print(ls())


# ------------------------------------------------------------
# Exercise
# ------------------------------------------------------------
# (a) First 20 items in methods package; total count
cat("\n9.1(a) First 20 items in package:methods:\n")
items_methods <- ls("package:methods")
print(head(items_methods, 20))
cat("Total items in package:methods: ")
print(length(items_methods))

# (b) Determine owning environment (namespace) for each function
cat("\n9.1(b) Owning environments:\n")
cat("read.table -> "); print(environment(read.table))
cat("data       -> "); print(environment(data))
cat("matrix     -> "); print(environment(matrix))
cat("jpeg       -> "); print(environment(jpeg))

# (c) Confirm smoothScatter is part of graphics package
cat("\n9.1(c) Confirm smoothScatter in graphics package:\n")
graphics_items <- ls("package:graphics")
print(any(graphics_items == "smoothScatter"))


# ------------------------------------------------------------
# 9.2 Argument Matching
# ------------------------------------------------------------
cat("\n--- 9.2 Argument Matching ---\n")

# -------------------------
# 9.2.1 Exact matching
# -------------------------
cat("\n9.2.1 Exact matching examples:\n")
bar <- matrix(data = 1:9, nrow = 3, ncol = 3,
              dimnames = list(c("A", "B", "C"), c("D", "E", "F")))
print(bar)

# Order doesn't matter with exact tags
bar <- matrix(nrow = 3,
              dimnames = list(c("A", "B", "C"), c("D", "E", "F")),
              ncol = 3,
              data = 1:9)
print(bar)

# -------------------------
# 9.2.2 Partial matching
# -------------------------
cat("\n9.2.2 Partial matching examples:\n")
bar <- matrix(nr = 3, di = list(c("A", "B", "C"), c("D", "E", "F")), nc = 3, dat = 1:9)
print(bar)

# Ambiguous partial tag example (should error)
cat("\nAmbiguous partial matching example (handled with try):\n")
try(matrix(nr = 3, di = list(c("A", "B", "C"), c("D", "E", "F")), nc = 3, d = 1:9), silent = TRUE)

# -------------------------
# 9.2.3 Positional matching
# -------------------------
cat("\n9.2.3 Positional matching examples:\n")
cat("args(matrix):\n")
print(args(matrix))

bar <- matrix(1:9, 3, 3, FALSE, list(c("A", "B", "C"), c("D", "E", "F")))
print(bar)

# Wrong positional usage (missing byrow) -> error
cat("\nPositional matching error example (handled with try):\n")
try(matrix(1:9, 3, 3, list(c("A", "B", "C"), c("D", "E", "F"))), silent = TRUE)

# -------------------------
# 9.2.4 Mixed matching
# -------------------------
cat("\n9.2.4 Mixed matching example:\n")
bar <- matrix(1:9, 3, 3, dim = list(c("A", "B", "C"), c("D", "E", "F")))
print(bar)

# -------------------------
# 9.2.5 Ellipsis (...)
# -------------------------
cat("\n9.2.5 Ellipsis examples:\n")
cat("args(data.frame):\n")
print(args(data.frame))

cat("args(plot):\n")
print(args(plot))

# Example: data.frame uses ... for its main ingredients
DF <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
print(DF)

# Example: plot uses ... for optional graphical parameters
# (creates a quick plot if interactive; safe in non-interactive too)
# Uncomment if you want to see a plot:
# plot(1:5, 1:5, pch = 19, col = "blue", main = "Demo", cex = 1.2)


# ------------------------------------------------------------
# Exercise 
# ------------------------------------------------------------
# (a) Positional matching with seq: from -4 to 4 by 0.2
# Usage: seq(from, to, by) -- by is 3rd argument
seq_a <- seq(-4, 4, 0.2)
cat("\n9.2(a) seq(-4, 4, 0.2):\n")
print(head(seq_a))
print(tail(seq_a))

# (b) Identify matching styles
# We print the calls and annotate in comments.
cat("\n9.2(b) Matching styles (see comments in script):\n")

# i) array(8:1, dim=c(2,2,2))  -> mixed (positional for data, exact for dim)
obj_i <- array(8:1, dim = c(2, 2, 2))
print(obj_i)

# ii) rep(1:2, 3) -> positional (x, times)
obj_ii <- rep(1:2, 3)
print(obj_ii)

# iii) seq(from=10, to=8, length=5) -> exact (named)
obj_iii <- seq(from = 10, to = 8, length = 5)
print(obj_iii)

# iv) sort(decreasing=T, x=c(...)) -> mixed (exact for decreasing and x)
obj_iv <- sort(decreasing = TRUE, x = c(2, 1, 1, 2, 0.3, 3, 1.3))
print(obj_iv)

# v) which(matrix(c(T,F,T,T),2,2)) -> positional inside matrix, positional in which
obj_v <- which(matrix(c(TRUE, FALSE, TRUE, TRUE), 2, 2))
print(obj_v)

# vi) which(matrix(c(T,F,T,T),2,2), a=T) -> error (which has no arg 'a'); demonstrate via try
cat("\n9.2(b)(vi) invalid arg name example (handled with try):\n")
try(which(matrix(c(TRUE, FALSE, TRUE, TRUE), 2, 2), a = TRUE), silent = TRUE)

# (c) plot.default ellipsis arguments
# Determine which of {type, pch, xlab, ylab, lwd, lty, col} are in plot.default formals.
cat("\n9.2(c) Which specified args are in plot.default formals vs in ... ?\n")
formals_pd <- names(formals(plot.default))
args_to_check <- c("type", "pch", "xlab", "ylab", "lwd", "lty", "col")

in_formals <- args_to_check[args_to_check %in% formals_pd]
in_dots    <- setdiff(args_to_check, in_formals)

cat("In plot.default formals (NOT via ...):\n")
print(in_formals)
cat("Likely passed via ... (not formal args of plot.default):\n")
print(in_dots)


# ============================================================
# Chapter 11: Writing Functions — Code Pack
# ============================================================

# ------------------------------------------------------------
# 11.1 The function Command
# ------------------------------------------------------------

# -------------------------
# 11.1.1 Function Creation (Fibonacci examples)
# -------------------------
cat("\n--- 11.1.1 Function Creation ---\n")

# myfib: prints Fibonacci terms until > 150 (no args)
myfib <- function(){
  fib.a <- 1
  fib.b <- 1
  cat(fib.a, ", ", fib.b, ", ", sep = "")
  repeat{
    temp <- fib.a + fib.b
    fib.a <- fib.b
    fib.b <- temp
    cat(fib.b, ", ", sep = "")
    if(fib.b > 150){
      cat("BREAK NOW...")
      break
    }
  }
}

cat("\nRun myfib():\n")
myfib(); cat("\n")

# myfib2: threshold controlled by argument
myfib2 <- function(thresh){
  fib.a <- 1
  fib.b <- 1
  cat(fib.a, ", ", fib.b, ", ", sep = "")
  repeat{
    temp <- fib.a + fib.b
    fib.a <- fib.b
    fib.b <- temp
    cat(fib.b, ", ", sep = "")
    if(fib.b > thresh){
      cat("BREAK NOW...")
      break
    }
  }
}

cat("\nRun myfib2(thresh=150):\n")
myfib2(thresh = 150); cat("\n")

cat("\nRun myfib2(1000000) (positional):\n")
myfib2(1000000); cat("\n")

# myfib3: returns a vector of Fibonacci numbers up to first > thresh
myfib3 <- function(thresh){
  fibseq <- c(1, 1)
  counter <- 2
  repeat{
    fibseq <- c(fibseq, fibseq[counter-1] + fibseq[counter])
    counter <- counter + 1
    if(fibseq[counter] > thresh){
      break
    }
  }
  return(fibseq)
}

cat("\nRun myfib3(150) (returns vector):\n")
print(myfib3(150))

foo <- myfib3(10000)
cat("\nAssign foo <- myfib3(10000); head/tail:\n")
print(head(foo)); print(tail(foo))

bar <- foo[1:5]
cat("\nbar <- foo[1:5]:\n")
print(bar)


# -------------------------
# 11.1.2 Using return
# -------------------------
cat("\n--- 11.1.2 Using return ---\n")

# dummy1: no explicit return; returns last evaluated object (dd)
dummy1 <- function(){
  aa <- 2.5
  bb <- "string me along"
  cc <- "string 'em up"
  dd <- 4:8
}

# dummy2: explicit return(dd)
dummy2 <- function(){
  aa <- 2.5
  bb <- "string me along"
  cc <- "string 'em up"
  dd <- 4:8
  return(dd)
}

# dummy3: returns early; later code never runs
# NOTE: As printed in your excerpt, there is a stray "return(bb)" outside the function.
# In valid R, return() must be inside a function.
dummy3 <- function(){
  aa <- 2.5
  bb <- "string me along"
  return(aa)
  cc <- "string 'em up"
  dd <- 4:8
  return(bb)
}

cat("\nfoo <- dummy1():\n")
foo <- dummy1(); print(foo)

cat("\nbar <- dummy2():\n")
bar <- dummy2(); print(bar)

cat("\nbaz <- dummy3() (returns aa only):\n")
baz <- dummy3(); print(baz)


# ------------------------------------------------------------
# Exercise
# ------------------------------------------------------------
# (a) myfib4: print or return depending on printme
myfib4 <- function(thresh, printme){
  fibseq <- c(1, 1)
  counter <- 2
  repeat{
    fibseq <- c(fibseq, fibseq[counter-1] + fibseq[counter])
    counter <- counter + 1
    if(fibseq[counter] > thresh) break
  }
  
  if(isTRUE(printme)){
    cat(paste(fibseq, collapse = ", "), ", BREAK NOW...\n", sep = "")
  } else {
    return(fibseq)
  }
}

cat("\nTest calls for 11.1(a):\n")
myfib4(thresh = 150, printme = TRUE)
myfib4(1000000, TRUE)
print(myfib4(150, FALSE))
print(myfib4(1000000, printme = FALSE))


# (b) myfac: factorial for non-negative integer (assume valid)
myfac <- function(int){
  # assumes int is a non-negative integer
  if(int == 0) return(1)
  out <- 1
  k <- 1
  while(k <= int){
    out <- out * k
    k <- k + 1
  }
  return(out)
}

cat("\n11.1(b)(i) myfac tests: 5!, 12!, 0!\n")
print(myfac(5))   # 120
print(myfac(12))  # 479001600
print(myfac(0))   # 1

# myfac2: if negative, return NaN
myfac2 <- function(int){
  if(int < 0) return(NaN)
  if(int == 0) return(1)
  out <- 1
  k <- 1
  while(k <= int){
    out <- out * k
    k <- k + 1
  }
  return(out)
}

cat("\n11.1(b)(ii) myfac2 tests: 5!, 12!, 0!, (-6)!\n")
print(myfac2(5))
print(myfac2(12))
print(myfac2(0))
print(myfac2(-6))


# ------------------------------------------------------------
# 11.2 Arguments
# ------------------------------------------------------------

# -------------------------
# 11.2.1 Lazy Evaluation
# -------------------------
cat("\n--- 11.2.1 Lazy Evaluation (multiples1) ---\n")

multiples1 <- function(x, mat, str1, str2){
  matrix.flags <- sapply(x, FUN = is.matrix)
  if(!any(matrix.flags)){
    return(str1)
  }
  indexes <- which(matrix.flags)
  counter <- 0
  result <- list()
  for(i in indexes){
    temp <- x[[i]]
    if(ncol(temp) == nrow(mat)){
      counter <- counter + 1
      result[[counter]] <- temp %*% mat
    }
  }
  if(counter == 0){
    return(str2)
  } else {
    return(result)
  }
}

# Test lists
foo <- list(matrix(1:4, 2, 2), "not a matrix",
            "definitely not a matrix", matrix(1:8, 2, 4), matrix(1:8, 4, 2))
bar <- list(1:4, "not a matrix", c(FALSE, TRUE, TRUE, TRUE), "??")
baz <- list(1:4, "not a matrix", c(FALSE, TRUE, TRUE, TRUE), "??", matrix(1:8, 2, 4))

cat("\nRun multiples1 on foo (needs no str1/str2 due to lazy eval):\n")
print(multiples1(x = foo, mat = diag(2), str1 = "no matrices in 'x'",
                 str2 = "matrices in 'x' but none of appropriate dimensions given 'mat'"))

cat("\nRun multiples1 on bar (returns str1):\n")
print(multiples1(x = bar, mat = diag(2), str1 = "no matrices in 'x'",
                 str2 = "matrices in 'x' but none of appropriate dimensions given 'mat'"))

cat("\nRun multiples1 on baz (returns str2):\n")
print(multiples1(x = baz, mat = diag(2), str1 = "no matrices in 'x'",
                 str2 = "matrices in 'x' but none of appropriate dimensions given 'mat'"))

cat("\nLazy call: multiples1(x=foo, mat=diag(2)) works (str1/str2 not needed):\n")
print(multiples1(x = foo, mat = diag(2)))

cat("\nLazy call: multiples1(x=bar, mat=diag(2)) errors (str1 needed) (handled with try):\n")
try(print(multiples1(x = bar, mat = diag(2))), silent = TRUE)


# -------------------------
# 11.2.2 Setting Defaults (multiples2)
# -------------------------
cat("\n--- 11.2.2 Setting Defaults (multiples2) ---\n")

multiples2 <- function(x, mat, str1 = "no valid matrices", str2 = str1){
  matrix.flags <- sapply(x, FUN = is.matrix)
  if(!any(matrix.flags)){
    return(str1)
  }
  indexes <- which(matrix.flags)
  counter <- 0
  result <- list()
  for(i in indexes){
    temp <- x[[i]]
    if(ncol(temp) == nrow(mat)){
      counter <- counter + 1
      result[[counter]] <- temp %*% mat
    }
  }
  if(counter == 0){
    return(str2)
  } else {
    return(result)
  }
}

cat("\nmultiples2(foo, diag(2)):\n")
print(multiples2(foo, mat = diag(2)))
cat("\nmultiples2(bar, diag(2)):\n")
print(multiples2(bar, mat = diag(2)))
cat("\nmultiples2(baz, diag(2)):\n")
print(multiples2(baz, mat = diag(2)))


# -------------------------
# 11.2.3 Checking for Missing Arguments (multiples3)
# -------------------------
cat("\n--- 11.2.3 missing() checks (multiples3) ---\n")

multiples3 <- function(x, mat, str1, str2){
  matrix.flags <- sapply(x, FUN = is.matrix)
  if(!any(matrix.flags)){
    if(missing(str1)){
      return("'str1' was missing, so this is the message")
    } else {
      return(str1)
    }
  }
  indexes <- which(matrix.flags)
  counter <- 0
  result <- list()
  for(i in indexes){
    temp <- x[[i]]
    if(ncol(temp) == nrow(mat)){
      counter <- counter + 1
      result[[counter]] <- temp %*% mat
    }
  }
  if(counter == 0){
    if(missing(str2)){
      return("'str2' was missing, so this is the message")
    } else {
      return(str2)
    }
  } else {
    return(result)
  }
}

cat("\nmultiples3(foo, diag(2)):\n")
print(multiples3(foo, diag(2)))
cat("\nmultiples3(bar, diag(2)) (str1 missing):\n")
print(multiples3(bar, diag(2)))
cat("\nmultiples3(baz, diag(2)) (str2 missing):\n")
print(multiples3(baz, diag(2)))


# -------------------------
# 11.2.4 Dealing with Ellipses
# -------------------------
cat("\n--- 11.2.4 Ellipses (...) ---\n")

myfibplot <- function(thresh, plotit = TRUE, ...){
  fibseq <- c(1, 1)
  counter <- 2
  repeat{
    fibseq <- c(fibseq, fibseq[counter-1] + fibseq[counter])
    counter <- counter + 1
    if(fibseq[counter] > thresh) break
  }
  
  if(isTRUE(plotit)){
    # Plotting is safe even if non-interactive; it will open a device if available.
    plot(1:length(fibseq), fibseq, ...)
  } else {
    return(fibseq)
  }
}

cat("\nmyfibplot(150) default plot (commented out by default).\n")
# Uncomment if you want to see plots:
# myfibplot(150)
# myfibplot(150, type="b", pch=4, lty=2,
#           main="Terms of the Fibonacci sequence",
#           ylab="Fibonacci number", xlab="Term (n)")

# Unpacking ellipsis
unpackme <- function(...){
  x <- list(...)
  cat("Here is ... in its entirety as a list:\n")
  print(x)
  cat("\nThe names of ... are:", names(x), "\n")
  cat("\nThe classes of ... are:", sapply(x, class), "\n")
}

cat("\nRun unpackme(...) demo:\n")
unpackme(
  aa = matrix(1:4, 2, 2),
  bb = TRUE,
  cc = c("two", "strings"),
  dd = factor(c(1, 1, 2, 1))
)


# ------------------------------------------------------------
# Exercise
# ------------------------------------------------------------

# (a) Compound interest function with optional step plot and ellipsis
compound_interest <- function(P, i, t = 12, y, plotit = TRUE, ...){
  # Validate missing required arguments
  if(missing(P) || missing(i) || missing(y)){
    stop("P, i, and y must be supplied")
  }
  # Integer time points 1..y
  yrs <- 1:y
  F <- P * (1 + i/(100 * t))^(t * yrs)
  
  if(isTRUE(plotit)){
    plot(yrs, F, type = "s", xlab = "Years", ylab = "Amount", ...)
  } else {
    return(F)
  }
}

cat("\n11.2(a)(i) $5000, 4.4% p.a., monthly, 10 years (return vector):\n")
res_i <- compound_interest(P = 5000, i = 4.4, t = 12, y = 10, plotit = FALSE)
print(res_i)

cat("\n11.2(a)(ii) Recreate plot: $100 at 22.9% p.a., monthly, 20 years (plot commented):\n")
# Uncomment to plot
# compound_interest(P = 100, i = 22.9, t = 12, y = 20, plotit = TRUE,
#                   main = "Compound interest: monthly vs annual",
#                   col = "blue", lwd = 2)

cat("\n11.2(a)(iii) Same as (ii) but compounded annually; add as second step line + legend (code commented):\n")
res_annual <- compound_interest(P = 100, i = 22.9, t = 1, y = 20, plotit = FALSE)
# Uncomment to add to the previous plot:
# lines(1:20, res_annual, type = "s", col = "red", lwd = 2, lty = 2)
# legend("topleft", legend = c("Monthly", "Annual"),
#        col = c("blue", "red"), lty = c(1, 2), lwd = 2, bty = "n")


# (b) Quadratic solver with missing checks
quadsolve <- function(k1, k2, k3){
  if(missing(k1) || missing(k2) || missing(k3)){
    return("Missing k1, k2, or k3: cannot compute roots")
  }
  disc <- k2^2 - 4 * k1 * k3
  if(disc < 0){
    cat("No real roots (discriminant < 0)\n")
    return(numeric(0))
  } else if(disc == 0){
    return(-k2 / (2 * k1))
  } else {
    r1 <- (-k2 - sqrt(disc)) / (2 * k1)
    r2 <- (-k2 + sqrt(disc)) / (2 * k1)
    return(c(r1, r2))
  }
}

cat("\n11.2(b)(i) Confirm roots for 2x^2 - x - 5:\n")
print(quadsolve(2, -1, -5))

cat("\n11.2(b)(i) x^2 + x + 1 has no real roots:\n")
print(quadsolve(1, 1, 1))

cat("\n11.2(b)(ii) More tests:\n")
print(quadsolve(1.3, -8, -3.13))
print(quadsolve(2.25, -3, 1))
print(quadsolve(1.4, -2.2, -5.1))
print(quadsolve(-5, 10.11, -9.9))

cat("\n11.2(b)(iii) Missing argument test:\n")
print(quadsolve(k1 = 1, k2 = 2))


# ------------------------------------------------------------
# 11.3 Specialized Functions
# ------------------------------------------------------------

# -------------------------
# 11.3.1 Helper Functions
# -------------------------
cat("\n--- 11.3.1 Helper Functions ---\n")

multiples_helper_ext <- function(x, matrix.flags, mat){
  indexes <- which(matrix.flags)
  counter <- 0
  result <- list()
  for(i in indexes){
    temp <- x[[i]]
    if(ncol(temp) == nrow(mat)){
      counter <- counter + 1
      result[[counter]] <- temp %*% mat
    }
  }
  return(list(result, counter))
}

multiples4 <- function(x, mat, str1 = "no valid matrices", str2 = str1){
  matrix.flags <- sapply(x, FUN = is.matrix)
  if(!any(matrix.flags)){
    return(str1)
  }
  helper.call <- multiples_helper_ext(x, matrix.flags, mat)
  result <- helper.call[[1]]
  counter <- helper.call[[2]]
  if(counter == 0){
    return(str2)
  } else {
    return(result)
  }
}

cat("\nmultiples4(foo, diag(2)):\n")
print(multiples4(foo, diag(2)))

# Internally defined helper
multiples5 <- function(x, mat, str1 = "no valid matrices", str2 = str1){
  matrix.flags <- sapply(x, FUN = is.matrix)
  if(!any(matrix.flags)){
    return(str1)
  }
  
  multiples_helper_int <- function(x, matrix.flags, mat){
    indexes <- which(matrix.flags)
    counter <- 0
    result <- list()
    for(i in indexes){
      temp <- x[[i]]
      if(ncol(temp) == nrow(mat)){
        counter <- counter + 1
        result[[counter]] <- temp %*% mat
      }
    }
    return(list(result, counter))
  }
  
  helper.call <- multiples_helper_int(x, matrix.flags, mat)
  result <- helper.call[[1]]
  counter <- helper.call[[2]]
  if(counter == 0){
    return(str2)
  } else {
    return(result)
  }
}

cat("\nmultiples5(foo, diag(2)):\n")
print(multiples5(foo, diag(2)))


# -------------------------
# 11.3.2 Disposable (Anonymous) Functions
# -------------------------
cat("\n--- 11.3.2 Disposable Functions (apply with function(x){...}) ---\n")

foo_mat <- matrix(c(2,3,3,4,2,4,7,3,3,6,7,2), 3, 4)
cat("\nfoo_mat:\n")
print(foo_mat)

cat("\nApply: repeat each column twice and sort:\n")
res_apply <- apply(foo_mat, MARGIN = 2, FUN = function(x){ sort(rep(x, 2)) })
print(res_apply)


# -------------------------
# 11.3.3 Recursive Functions
# -------------------------
cat("\n--- 11.3.3 Recursive Functions ---\n")

myfibrec <- function(n){
  if(n == 1 || n == 2){
    return(1)
  } else {
    return(myfibrec(n-1) + myfibrec(n-2))
  }
}

cat("\nmyfibrec(5):\n")
print(myfibrec(5))


# ------------------------------------------------------------
# Exercise 
# ------------------------------------------------------------

# (a) Disposable function with lapply: add "!" to every element of every member
foo_list <- list("a", c("b", "c", "d", "e"), "f", c("g", "h", "i"))
cat("\n11.3(a) Original list:\n")
print(foo_list)

cat("\n11.3(a) After adding ! with lapply + disposable function:\n")
print(lapply(foo_list, function(x){ paste(x, "!", sep = "") }))


# (b) Recursive factorial for non-negative integers
# NOTE: Your excerpt lists "120 factorial is 479,001,600" which is actually 12 factorial.
# We'll test 5!, 12!, and 0!.
myfacrec <- function(n){
  if(n < 0) return(NaN)
  if(n == 0) return(1)
  return(n * myfacrec(n - 1))
}

cat("\n11.3(b) Recursive factorial tests:\n")
print(myfacrec(5))   # 120
print(myfacrec(12))  # 479001600
print(myfacrec(0))   # 1


# (c) geolist: geometric mean per member; helper function inside
geolist <- function(x){
  
  geo_mean <- function(v){
    # Assumes numeric vector, length >= 1, positive values for standard geometric mean
    # If zeros or negatives occur, geometric mean may be undefined in reals.
    prod(v)^(1/length(v))
  }
  
  for(i in seq_along(x)){
    item <- x[[i]]
    
    if(is.matrix(item)){
      # geometric mean of each row
      x[[i]] <- apply(item, 1, FUN = function(row){ geo_mean(row) })
    } else {
      # treat as vector
      x[[i]] <- geo_mean(item)
    }
  }
  
  return(x)
}

cat("\n11.3(c)(i) Test case 1:\n")
foo <- list(
  1:3,
  matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4), 4, 2),
  matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4), 2, 4)
)
print(geolist(foo))

cat("\n11.3(c)(ii) Test case 2:\n")
bar <- list(
  1:9,
  matrix(1:9, 1, 9),
  matrix(1:9, 9, 1),
  matrix(1:9, 3, 3)
)
print(geolist(bar))


