#!/usr/bin/python

class RPNSolver:
    '''Reverse Polish Notation Formula Solver.
    
    Supported operations are +, -, *, / and ^.'''
    stack = []

    def solve(self, e):
        del self.stack[:]
        items = e.split(' ')
        for item in items:
            if item == '+':
                if not self.compute('+'):
                    break
            elif item == '-':
                if not self.compute('-'):
                    break
            elif item == '*':
                if not self.compute('*'):
                    break
            elif item == '/':
                if not self.compute('/'):
                    break
            elif item == '^':
                if not self.compute('^'):
                    break
            else:
                self.stack.append(float(item))

        correct = len(self.stack)
        if correct == 0:
            print 0.0
        elif correct == 1:
            print self.stack[0]
        else:
            print "Error"

    def compute(self, op):
        if len(self.stack) < 2:
            print "Unrecognizable input"
            return False
        res = 0.0
        if op == '+':
            res = self.stack[-2] + self.stack[-1]
        elif op == '-':
            res = self.stack[-2] - self.stack[-1] 
        elif op == '*':
            res = self.stack[-2] * self.stack[-1]
        elif op == '/':
            res = self.stack[-2] / self.stack[-1]
        elif op == '^':
            res = self.stack[-2] ** self.stack[-1]
        else:
            print "Unrecognizable operator"
            return False
        del self.stack[-1]
        del self.stack[-1]
        self.stack.append(res)
        return True

# test part
rpn = RPNSolver()
print rpn.__doc__
while True:
    s = raw_input(">>")
    if s == "quit":
        break
    rpn.solve(s)
print "Auf wiedersehen!"
