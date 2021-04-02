project('bcc', 'c')

src = [
  'src/utils.c',
  'src/error.c',
  'src/pp.c',
  'src/lexer.c',
  'src/parser.c',
  'src/sem_names.c',
  'src/sem_types.c',
  'src/sem_return.c',
  'src/gen_stack.c',
  'src/codegen.c',
  'src/main.c'
]

inc = include_directories('include')

executable(
  'bcc',
  src,
  c_args : ['-Wextra', '-Werror'],
  include_directories : [inc],
)
