define(`Pprint',`System.out.println($1)')
define(`locPrint', `System.out.println("on line: __line__")')
define(`ppnulltest', `if($1 == null) { throw new IllegalArgumentException("$1 should not be null"); }')
