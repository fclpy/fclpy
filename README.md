# fclpy - Fusion Common Lisp for Python

A common lisp interpreter fully integrated with python.

# Installation

```
pip install fclpy
```

# Usage

```
import fclpy as common_lisp
```


# Project Goals 

Order from most important to least concern

1. Interpreter running natively in python
1. All lisp entities (functions,types,variables,classes, etc) can be accessed directly from python
1. Standards Compliance
1. Resource limitations to prevent LISP applications from freezing the python interpreter
1. Ability to safely run untrusted code (sandboxing) 
1. High Performance

# Founders

* Ralph Ritoch

# Development

This project is too important, and too big, to be handled by any one developer.

This project is hosted at Github 

* http://www.github.com/fclpy/fclpy

Development is being managed on Azure DevOps

* https://dev.azure.com/rritoch/fclpy

# License

MIT License

Copyright (c) 2019 Ralph Ritoch

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.