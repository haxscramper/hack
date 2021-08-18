#define empty_macro
empty_macro // empty_macro

#define no_args result
// no args, in comment: no_args
"no args in string: no_args"
no_args // no_args

#define one_arg(arg) arg
one_arg("exs") // one argument 

#define nested_parens(arg) arg
nested_parens((((12)))) // nested parens

#define two_args(arg1, arg2) arg1 -- arg2
// two args
two_args(1, 2) // two_args(1, 2)
two_args((1), (2)) // two_args((1), (2))
two_args((1) (,2)) // does not work, expands to `two_args`, discards arguments
         // 'too few arguments in invocation of macro "two_args"'
two_args((1, 3), (2))      

two_args((((((1))))), (((((3)))))) // two_args((((((1))))), (((((3))))))

#define concat_args(arg1, arg2) #arg1 #arg2
concat_args(1, 2) // concat_args(1, 2)
concat_args(1, (2)) // concat_args(1, (2))
concat_args((1), (2)) // concat_args((1), (2))

#define concat_tokens(arg1, arg2) arg1 ## arg2
concat_tokens(1, 2) // concat_tokens(1, 2)
concat_tokens(1, (2)) // concat_tokens(1, (2))
concat_tokens((1), (2)) // concat_tokens((1), (2))

#define COMMA ,
#define expand_args1(arg1, arg2) arg1 arg2

expand_args1(1 COMMA 2, 3) // expand_args1(1 COMMA 2, 3)

#define expand_args2(arg1, arg2) expand_args1(arg1) arg2
expand_args2(1 COMMA 2, 3) // expand_args2(1 COMMA 2, 3)