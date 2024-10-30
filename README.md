# 🗄️ My Scala DataBase


Implemented a **Query Language** inspired from `SQL`,
which uses a DataBase implemented by me, written in Scala.


The source code keeps a **Functional Paradigm**
(variables are constants and immutable).
It leverages extension methods and implicit conversions to simplify function calls, 
providing an elegant and easy-to-understand syntax for queries.




## Tables

My DataBases reads a `CSV` file and stores data in a table-like data structure, for example:

| First name | Last name | Age  |
| :---       | :---      | :--- |
| John       | Doe       | 30   |
| Jane       | Smith     | 25   |



This was represented in code by:

```scala
type Row = Map[String, String] // column_name -> value
type Tabular = List[Row]
```


Furthermore, I even extended Scala's functionalities and definied a database table as a class:

```scala
case class Table(tableName: String, tableData: Tabular) {
  def header: List[String]
  def data: Tabular
  def name: String
}
```

A user can do the following operations on a `Table` instance:
- `insert`
- `delete`
- `sort`
- `select`
- `apply`


```scala
case class Table(tableName: String, tableData: Tabular) {
    ...
    def insert(row: Row): Table
    def delete(row: Row): Table
    def sort(column: String): Table
    def select(columns: List[String]): Table
}
```




## Filtering a Table

Futhermore, the user can apply multiple `filters` on the `Table` instances:
```
 <filter> ::= 
    <filter> && <filter> | 
    <filter> || <filter> |
    <filter> == <filter> |
    !<filter> |
    any [ <filter> ] |
    all [ <filter> ] |
    operation [ <filter> ]
```



```scala
trait FilterCond { def eval(r: Row): Option[Boolean] }
```
```scala
case class Field(colName: String, predicate: String => Boolean) extends FilterCond {
  override def eval(r: Row): Option[Boolean]
}
 
case class Compound(op: (Boolean, Boolean) => Boolean, conditions: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean]
}
 
case class Not(f: FilterCond) extends FilterCond {
  override def eval(r: Row): Option[Boolean]
}
 
def And(f1: FilterCond, f2: FilterCond): FilterCond
def Or(f1: FilterCond, f2: FilterCond): FilterCond
def Equal(f1: FilterCond, f2: FilterCond): FilterCond
 
case class Any(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean]
}
 
case class All(fs: List[FilterCond]) extends FilterCond {
  override def eval(r: Row): Option[Boolean]
}
```
```scala
extension (f: FilterCond) {
  def ==(other: FilterCond)
  def &&(other: FilterCond)
  def ||(other: FilterCond)
  def !! = ??
}
```



## DataBase


A database contains one or more tables, on which a user can apply a series of operations:
- `create`: adds a new empty table in the database
- `drop`: removes a talbe by name from the database
- `selectTables`: extracts tables from the database, by specified named
- `join`: takes two tables and a designated column from each.
It will merge the two tables according to the specified columns.


> The `join` function is a **full outer join**.


First of all, I defined a `Database` as a **list of** `Tables`.

```scala
case class Database(tables: List[Table]) {
    def create(tableName: String): Database
    def drop(tableName: String): Database
    def selectTables(tableNames: List[String]): Option[Database]
    def join(table1: String, c1: String, table2: String, c2: String): Option[Table]
}
```


## Query Languge API



I developed a **query language** that will serve as an **API**
for a wide range of table transformations, previously implemented as functions.
This **query language** will allow sequences or combinations of these transformations.

In the implementation of the query language,
I focussed on including functionalities similar to those in SQL,
as well as on error handling.

The language will allow two main categories of operations:

- operations on the entire database
- operations on a single table

For error handling, I will use the `Option` **ADT** (Abstract Data Type),
where `Some(_)` indicates a valid result of an operation, while `None` signals an error.
If a query generates an **error**, it will **propagate** if the result is necessary for the execution of another query.


## Testing


In order to test the source code,
simply execute the following command in your terminal
from the root directory of this git repo:

```sh
$ sbt test
```
