# This example defines a simple expression tree for arithmetic.
# We then define a function (class methods) to convert any natural
# number into a unique expression instance.

# We're going to use the Rosenberg-strong pairing function as our building
# block for our expression tree.  

# pair: (N, N) -> N
def pair(x, y):
    mxy = max(x, y)
    return mxy * mxy + mxy + x - y

# unpair: N -> (N, N)
def unpair(z):
    m = int(z ** 0.5)
    ml = z - m * m
    if ml < m:
        return ml, m
    else:
        return m, m * m + m + m - z


# Define an AST for simple arithmetic equations
class Expression:
    @classmethod
    def from_num(cls, value):
        # Pick the operation using modulus
        maxType = 5  # 5 is the number of operations.  You can lower this if you want
                     # to see how the tree grows with fewer operations.
        
        # We'll use the modulus to pick the operation
        m = value % maxType
        # We'll use the remainder to pick the operands
        value //= maxType
        if m == 0:
            return Num.from_num(value)
        elif m == 1:
            return Add.from_num(value)
        elif m == 2:
            return Sub.from_num(value)
        elif m == 3:
            return Mult.from_num(value)
        elif m == 4:
            return Div.from_num(value)

class Num(Expression):
    # Value must be a number
    def __init__(self, value):
        self.value = value
    # Define a constructor for the AST given a number
    @classmethod
    def from_num(cls, value):
        return cls(value)  # Numbers just map to themselves
    # Evaluate the number
    def eval(self):
        return self.value
    # Convert to string
    def __str__(self):
        return str(self.value)

class Add(Expression):
    def __init__(self, left, right):
        self.left = left
        self.right = right
    @classmethod
    def from_num(cls, value):
        # Generate two operands using unpairing function
        l, r = unpair(value)
        # Create the left and right operands and return the addition expression
        return cls(Expression.from_num(l), Expression.from_num(r))
    def eval(self):
        return self.left.eval() + self.right.eval()
    def __str__(self):
        return "(" + str(self.left) + " + " + str(self.right) + ")"

class Sub(Expression):
    def __init__(self, left, right):
        self.left = left
        self.right = right
    @classmethod
    def from_num(cls, value):
        # Generate two operands using unpairing function
        l, r = unpair(value)
        # Create the left and right operands and return the multiplication expression
        return cls(Expression.from_num(l), Expression.from_num(r))
    def eval(self):
        return self.left.eval() - self.right.eval()
    def __str__(self):
        return "(" + str(self.left) + " - " + str(self.right) + ")"

class Mult(Expression):
    def __init__(self, left, right):
        self.left = left
        self.right = right
    @classmethod
    def from_num(cls, value):
        l, r = unpair(value)
        return cls(Expression.from_num(l), Expression.from_num(r))
    def eval(self):
        return self.left.eval() * self.right.eval()
    def __str__(self):
        return "(" + str(self.left) + " * " + str(self.right) + ")"

class Div(Expression):
    def __init__(self, left, right):
        self.left = left
        self.right = right
    @classmethod
    def from_num(cls, value):
        l, r = unpair(value)
        return cls(Expression.from_num(l), Expression.from_num(r))
    def eval(self):
        r = self.right.eval()
        
        # TODO:  WE'RE CHEATING HERE AND EVAL WILL NOT WORK PROPERLY
        if r == 0:
            return 0
        return self.left.eval() / r
    def __str__(self):
        return "(" + str(self.left) + " / " + str(self.right) + ")"

# Define the comparison operators in an array
operations = [
    "==",
    "!=",
    "<",
    ">",
    "<=",
    ">="
]

class Equation():
    # The left and right sides of the equation must be expressions
    # Comparison can be done with ==, !=, <, >, <=, >=
    def __init__(self, comparison, left, right):
        self.comparison = comparison
        self.left = left
        self.right = right
    @classmethod
    def from_num(cls, value):
        # Pick the comparison using modulus
        maxOp = 2
        opN = value % maxOp
        op = operations[opN]
        value //= maxOp
        l, r = unpair(value)
        left = Expression.from_num(l)
        right = Expression.from_num(r)
        return cls(op, left, right)
    def eval(self):
        # Eval the left and right sides
        left = self.left.eval()
        right = self.right.eval()
        # Compare the left and right sides
        if self.comparison == "==":
            return left == right
        elif self.comparison == "!=":
            return left != right
        elif self.comparison == "<":
            return left < right
        elif self.comparison == ">":
            return left > right
        elif self.comparison == "<=":
            return left <= right
        elif self.comparison == ">=":
            return left >= right
        else:
            raise Exception("Unknown comparison: " + self.comparison)

    def __str__(self):
        return str(self.left) + " " + str(self.comparison) + " " + str(self.right)

# Loop over 0..100 and print the pair and unpair of each number
for i in range(0, 100):
    eq = Equation.from_num(i)
    print(i, "---", eq.eval(), "---", eq)
