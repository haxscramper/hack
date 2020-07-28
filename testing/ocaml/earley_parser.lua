-- Copyright: Consider this file public domain.  Attribution would be
-- nice, though not required.

-- Usage: just execute `lua recogniser.lua` in the command line.
-- You probably want to tinker with the grammar and the input at the
-- end of this file.

-- Prerequisites: imperative programming and basic knowledge of Earley
-- parsing. The tutorial at
--   <http://loup-vaillant.fr/tutorials/earley-parsing/>
-- is highly recomended.
-- You don't need to know Lua.  Think of it as "executable pseudocode".


---------------------
-- Data Structures --
---------------------

-- Earley items are basically 3 integers.  I have represented them
-- with Lua tables:
local earley_item = {
   rule  = index_of_rule_in_grammar,
   next  = index_of_symbol_in_rule,
   start = start_position_in_input,
}
-- There are cleverer representation.  This one is merely the dumbest
-- I came up with.

-- Earley sets are dynamic arrays of Earley items.  They are
-- represented with Lua tables.  Warning: In Lua, array indices start
-- at 1.
-- S is the whole state of an Earley parsing.  It's a dynamic array of
-- Earley sets.  Again, they are represented with Lua tables.

-- Non-terminal symbols are represented with strings.
-- Terminal symbols are represented with functions:
local function  char(...) return ... end
local function class(...) return ... end
local function range(...) return ... end

-- Rules are Lua tables. Note: unnamed elements are implicitely
-- numbered.  That helps to represent arrays.  Warning: Lua arrays
-- start at 1.
-- So, the following lines represent the same rule:
local my_rule = { name = 'Sum',       'Sum',       class('+-'),       'Product'}
local my_rule = { name = 'Sum', [1] = 'Sum', [2] = class('+-'), [3] = 'Product'}

-- Grammars are a list of rules, and a name for the start rule.  They
-- are represented with...  Lua tables.  again, notice the unnamed
-- elements.
local example_grammar = {
   start_rule_name = 'Sum',
   { name = 'Sum'    , 'Sum'      , class('+-'), 'Product'},
   { name = 'Sum'    , 'Product'  ,                       },
   { name = 'Product', 'Product'  , class('*/'), 'Factor' },
   { name = 'Product', 'Factor'   ,                       },
   { name = 'Factor' , char('(')  , 'Sum', char(')')      },
   { name = 'Factor' , 'Number'                           },
   { name = 'Number' , range('09'), 'Number'              },
   { name = 'Number' , range('09'),                       },
}

-- (note: all the code above is dead. The real deal starts now.)


--------------------
-- Test utilities --  (Don't read this yet, it's not very interesting.)
--------------------

function has_partial_parse(S, i, grammar)
   local set = S[i]
   for i = 1, #set do
      local item = set[i]
      local rule  = grammar[item.rule]
      if rule.name  == grammar.start_rule_name and
         item.next  >  #rule                   and
         item.start == 1
      then
         return true
      end
   end
   return false
end

function has_complete_parse(S, grammar)
   return has_partial_parse(S, #S, grammar)
end

function last_partial_parse(S, grammar)
   for i = #S, 1, -1 do
      if has_partial_parse(S, i, grammar) then
         return i
      end
   end
   return nil
end

function diagnose(S, grammar, input)
   if has_complete_parse(S, grammar)
   then
      print("The input has been recognised. Congratulations!")
   else
      if #S == input:len() + 1
      then print("The whole input made sense. Maybe it is incomplete?")
      else io.write("The input stopped making sense at character ", #S, '\n')
      end

      local lpp = last_partial_parse(S, grammar)
      if lpp ~= nil
      then io.write("This begining of the input has been recognised: ",
                    input:sub(1, lpp - 1), '\n')
      else print("The begining of the input couldn't be parsed.")
      end
   end
end

---------------
-- Utilities -- (Read the function names and the comments.)
---------------
-- next element in the rule of this item
local function next_symbol(grammar, item)
   return grammar[item.rule][item.next]
end

-- gets the name of the rule pointed by the item
local function name(grammar, item)
   return grammar[item.rule].name
end

-- compares two items for equality (needed for safe append)
local function equal(item1, item2)
   return item1.rule  == item2.rule
      and item1.start == item2.start
      and item1.next  == item2.next
end

-- Adds an item at the end of the Earley set
local function unsafe_append(set, item)
   set[#set+1] = item
end

-- Adds an item at the end of the Earley set, **unless already present**
local function append(set, item)
   for i = 1, #set do
      if equal(item, set[i]) then
         return
      end
   end
   unsafe_append(set, item) -- the item wasn't already there, we add it
end

---------------------------
-- Detecting nullable rules -- Nullable symbols sets are named "nss".
---------------------------
local function nullable_nss()
   return { size = 0 }
end

local function add_nullable_rule(rule_name, nss)
   if nss[rule_name] then return end  -- Do nothing for known nullable rules.
   nss[rule_name] = true              -- The others are added,
   nss.size = nss.size + 1            -- and the size is ajusted.
end

-- Returns true if it can say for sure the rule is nullable.
-- Returns false otherwise
local function is_nullable(rule, nss)
   for i = 1, #rule do
      if not nss[rule[i]] then
         return false
      end
   end
   return true
end

-- Adds nullable rules to the nss, by examining them in one pass.
local function update_nss(nss, grammar)
   for i = 1, #grammar do                     -- For each rule,
      if is_nullable(grammar[i], nss) then       -- if the rule is nullable for sure,
         add_nullable_rule(grammar[i].name, nss) -- add it to the nss.
      end
   end
end

local function nullable_rules(grammar)
   local nss = nullable_nss()
   repeat                      -- Keep...
      local old_size = nss.size
      update_nss(nss, grammar) -- ...updating the nss,
   until old_size == nss.size  -- as long as it keeps growing.
   return nss                  -- An nss that stopped growing is complete.
end

---------------------------  ---------------------------  ----------------
-- Building Earley items --  This is the core algorithm.  Read everything.
---------------------------  ---------------------------  ----------------
local function complete(S, i, j, grammar)
   local item = S[i][j]
   for old_item_index, old_item in ipairs(S[item.start]) do
      if next_symbol(grammar, old_item) == name(grammar, item) then
         append(S[i], { rule  = old_item.rule,
                        next  = old_item.next + 1,
                        start = old_item.start })
      end
   end
end

local function scan(S, i, j, symbol, input)
   local item = S[i][j]
   if symbol(input, i) then -- terminal symbols are predicates
      if S[i+1] == nil then S[i+1]  = {} end
      unsafe_append(S[i+1], { rule  = item.rule,
                              next  = item.next + 1,
                              start = item.start })
   end
end

local function predict(S, i, j, symbol, grammar, nss)
   for rule_index, rule in ipairs(grammar) do
      if rule.name == symbol then
         append(S[i], { rule  = rule_index,
                        next  = 1 ,
                        start = i })
         if nss[rule.name] then -- magical completion
            append(S[i], { rule  = S[i][j].rule,
                           next  = S[i][j].next + 1,
                           start = S[i][j].start})
         end
      end
   end
end

local function build_items(grammar, input)
   -- Nullable rules detection
   local nss = nullable_rules(grammar)
   -- Earley sets
   local S = {{}}
   -- put start item(s) in S[1]
   for i = 1, #grammar do
      if grammar[i].name == grammar.start_rule_name then
         unsafe_append(S[1], { rule  = i,
                               start = 1,
                               next  = 1 })
      end
   end
   -- populate the rest of S[i]
   local i = 1
   while i <= #S do
      local j = 1
      while j <= #S[i] do
         local symbol = next_symbol(grammar, S[i][j])
         if     type(symbol) == "nil"      then complete(S, i, j, grammar)
         elseif type(symbol) == "function" then scan    (S, i, j, symbol, input)
         elseif type(symbol) == "string"   then predict (S, i, j, symbol, grammar, nss)
         else error("illegal rule")
         end
         j = j + 1
      end
      i = i + 1
   end
   return S
end

--------------
-- Printing -- (Don't read this yet, it's just utility code.)
--------------

-- Object that prints nicely laid out columns.
--
-- Usage:
--   pp = pretty_printer()
--   pp.line()
--     pp.col() pp.write('a', 42)
--     pp.col() pp.write(' |')
--   pp.line()
--     pp.col() pp.write("something") pp.write(" longer")
--     pp.col() pp.write(' |')
--     pp.col() pp.write(' another column')
--   pp.print()
--
-- result:
--   a42              |
--   something longer | another column
local function pretty_printer()
   local self = {}

   function self.write(...)
      local args = {...}
      for _, v in ipairs(args) do
         local l = self[#self] -- current line
         l[#l] = l[#l]..tostring(v)
      end
   end

   function self.col()
      local l = self[#self] -- current line
      l[#l+1] = ""
   end

   function self.line()
      self[#self+1] = {}
   end

   local function max(f)
      local m = 0
      for _, v in ipairs(self) do
         local x = f(v)
         if x > m then m = x end
      end
      return m
   end

   local function len(i)
      return function(v)
         if v[i] == nil
         then return 0
         else return v[i]:len()
         end
      end
   end

   local function nb_col(line) return #line end

   function self.print(indent)
      if indent == nil then indent = 0 end
      for i = 1, max(nb_col) do
         local max_len = max(len(i))
         for _, line in ipairs(self) do
            if line[i] == nil then line[i] = "" end
            line[i] = line[i]..string.rep(" ", max_len - line[i]:len())
         end
      end
      for _, line in ipairs(self) do
         io.write(string.rep(" ", indent))
         for _, col in ipairs(line) do
            io.write(col)
         end
         io.write('\n')
      end
   end

   return self
end

-- Prints all the Earley items.
local function print_S(S, grammar)
   for i, set in ipairs(S) do
      io.write('    === ', i-1, ' ===\n')
      pp = pretty_printer()
      for j, st in ipairs(set) do
         pp.line()
         pp.col() pp.write(name(grammar, st))
         pp.col() pp.write(' ->')
         for k, symbol in ipairs(grammar[st.rule]) do
            if k == st.next                   then pp.write(' •') end
            if     type(symbol) == "string"   then pp.write(' ', symbol)
            elseif type(symbol) == "function" then pp.write(' ', symbol())
            else                                   error("Impossible symbol")
            end
         end
         if st.next > #grammar[st.rule] then pp.write(' •') end
         pp.col() pp.write('  (', st.start-1, ')')
      end
      pp.print(4)
      io.write('\n')
   end
end

--------------------------------
-- Terminal symbol generators -- Do read this, but don't try too hard.
-------------------------------- There are less clever ways to do this.

-- The following functions generate terminal symbols.  A terminal
-- symbol is a function that tell us if the input matches them:
--    local semicolon = char(';')
--    print(semicolon(';')) -- true
--    print(semicolon('x')) -- false
--
-- They also come with an ugly hack to help us print them nicely:
--    print(semicolon()) -- ';'

local function char(c)
   return function(input, index)
      if input == nil
      then return "'"..c.."'" -- ugly hack used to print items
      else return input:byte(index) == c:byte()
      end
   end
end

local function range(r)
   return function(input, index)
      if input == nil
      then return "["..r:sub(1, 1)..'-'
                     ..r:sub(2, 2).."]" -- ugly hack used to print items
      else
         local i = input:byte(index)
         return i ~= nil
            and i >= r:byte(1)
            and i <= r:byte(2)
      end
   end
end

local function class(c)
   return function(input, index)
      if input == nil
      then return "["..c:sub(1, 1)
                     ..c:sub(2, 2).."]" -- ugly hack used to print items
      else
         return index <= input:len()
            and c:find(input:sub(index, index), 1, true) ~= nil
      end
   end
end

-------------------
-- Hello, world! --
-------------------
local Grammar = {
   start_rule_name = 'A',
   { name = 'A',    },
   { name = 'A', 'B'},
   { name = 'B', 'A'},
}
local input = ""
local S = build_items(Grammar, input)
io.write("Input: ", input, '\n') -- print the input
print_S(S, Grammar)              -- print all the internal state
diagnose(S, Grammar, input)      -- tell if the input is OK or not
