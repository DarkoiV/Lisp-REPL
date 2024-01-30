## Simple Lisp REPL
Simple REPL writen from scratch in haskell



### Build'n'run
To run, just clone, create build folder and execture run-repl.sh 
```bash
git clone https://github.com/DarkoiV/Lisp-REPL
cd Lisp-REPL
mkdir build
./run-repl.hs
```

### Basic functionality
Arthmetic operations in std lib  
![mathlisp](https://github.com/DarkoiV/Lisp-REPL/assets/25897608/3eb0f5fa-8da4-49e3-9b0c-29cbbf0cf53c)

Suport defining new values, including functions  
![my-func](https://github.com/DarkoiV/Lisp-REPL/assets/25897608/447f0c77-a54b-41e8-8db2-8e882578fb74)

And can do script files  
```lisp
(define greeting
  (lambda (name)
    (print "Welcome " name)))

(define ask-for-name
  (lambda ()
    (do
      (print "What is thou name?")
      (bind (name (read))
        (greeting name)))))

(print "Hello World!")
(ask-for-name)
```
![dofile](https://github.com/DarkoiV/Lisp-REPL/assets/25897608/d3fc7942-751b-4555-af8a-c978fa4cd6b2)
