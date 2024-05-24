import scala.language.implicitConversions
import scala.util.Try

trait PP_SQL_DB {
  def eval: Option[Database]
}


/**
 * apeleaza metoda `create` asociata unui `Database`
 */
case class CreateTable(database: Database, tableName: String) extends PP_SQL_DB {
  def eval: Option[Database] =
    Try(Some(database.create(tableName))).getOrElse(None)
}


/**
 * apeleaza metoda `drop` asociata unui `Database`
 */
case class DropTable(database: Database, tableName: String) extends PP_SQL_DB {
  def eval: Option[Database] =
    Try(Some(database.drop(tableName))).getOrElse(None)
}


/**
 * `implicit` -> conversia implicita a unei comenzi intr-un tabel
 *
 * @param t un tuplu cu 3 elemente:
 *            - tabelul din SQL
 *            - numele comenzii
 *            - parametrii comenzii (numele tabelului pt creare/stergere)
 * @return baza noua de date
 */
implicit def PP_SQL_DB_Create_Drop(t: (Option[Database], String, String)): Option[PP_SQL_DB] =
  t match {
    case (Some(db), "CREATE", tableName) => Some(CreateTable(db, tableName))
    case (Some(db), "DROP", tableName) => Some(DropTable(db, tableName))
    case _ => None
  }


/**
 * apeleaza metoda `select` asociata unui `Database`
 */
case class SelectTables(database: Database, tableNames: List[String]) extends PP_SQL_DB {
  def eval: Option[Database] =
    Try(database.selectTables(tableNames)).getOrElse(None)
}



/**
 * `implicit` -> conversia implicita a unei comenzi intr-un tabel
 *
 * @param t un tuplu cu 3 elemente:
 *            - tabelul din SQL
 *            - numele comenzii
 *            - parametrii comenzii (numele tabelului)
 * @return baza noua de date
 */
implicit def PP_SQL_DB_Select(t: (Option[Database], String, List[String])): Option[PP_SQL_DB] =
  t match {
    case (Some(db), "SELECT", tableNames) => Some(SelectTables(db, tableNames))
    case _ => None
  }





/**
 * apeleaza metoda `join` asociata unui `Database`
 * @param database
 * @param table1      numele primului tabel
 * @param column1     coloana pt JOIN
 * @param table2      numele celui de al doilea tabel
 * @param column2     coloana pt JOIN
 */
case class JoinTables(database: Database, table1: String, column1: String, table2: String, column2: String) extends PP_SQL_DB {
  def eval: Option[Database] =
    Try(database.join(table1, column1, table2, column2).map(table => Database(List(table)))).getOrElse(None)
}


/**
 * `implicit` -> conversia implicita a unei comenzi intr-un tabel
 *
 * @param t un tuplu cu 3 elemente:
 *            - tabelul din SQL
 *            - numele comenzii
 *            - parametrii comenzii JOIN
 * @return o baza de date
 */
implicit def PP_SQL_DB_Join(t: (Option[Database], String, String, String, String, String)): Option[PP_SQL_DB] =
  t match {
    case (Some(db), "JOIN", table1, col1, table2, col2) => Some(JoinTables(db, table1, col1, table2, col2))
    case _ => None
  }

trait PP_SQL_Table {
  def eval: Option[Table]
}

case class InsertRow(table: Table, values: Tabular) extends PP_SQL_Table {
  def eval: Option[Table] =
    Try(Some(values.foldLeft(table)((accTable, row) => accTable.insert(row)))).getOrElse(None)
}


/**
 * `implicit` -> conversia implicita a unei comenzi intr-un tabel
 *
 * @param t un tuplu cu 3 elemente:
 *            - tabelul din SQL
 *            - numele comenzii
 *            - parametrii comenzii INSERT
 * @return o baza de date
 */
implicit def PP_SQL_Table_Insert(t: (Option[Table], String, Tabular)): Option[PP_SQL_Table] =
  t match {
    case (Some(table), "INSERT", values) => Some(InsertRow(table, values))
    case _ => None
  }

case class UpdateRow(table: Table, condition: FilterCond, updates: Map[String, String]) extends PP_SQL_Table {
  def eval: Option[Table] =
    Try(Some(table.update(condition, updates))).getOrElse(None)
}


/**
 * `implicit` -> conversia implicita a unei comenzi intr-un tabel
 *
 * @param t un tuplu cu 3 elemente:
 *            - tabelul din SQL
 *            - numele comenzii
 *            - parametrii comenzii UPDATE (o conditie)
 * @return baza de date modificata
 */
implicit def PP_SQL_Table_Update(t: (Option[Table], String, FilterCond, Map[String, String])): Option[PP_SQL_Table] =
  t match {
    case (Some(table), "UPDATE", condition, updates) => Some(UpdateRow(table, condition, updates))
    case _ => None
  }

case class SortTable(table: Table, column: String) extends PP_SQL_Table {
  def eval: Option[Table] =
    Try(Some(table.sort(column))).getOrElse(None)
}


/**
 * `implicit` -> conversia implicita a unei comenzi intr-un tabel
 *
 * @param t un tuplu cu 3 elemente:
 *            - tabelul din SQL
 *            - numele comenzii
 *            - parametrii comenzii SORT (coloana in functie de care se realizeaza sortarea)
 * @return tabelul sortat in functie de o coloana
 */
implicit def PP_SQL_Table_Sort(t: (Option[Table], String, String)): Option[PP_SQL_Table] =
  t match {
    case (Some(table), "SORT", column) => Some(SortTable(table, column))
    case _ => None
  }

case class DeleteRow(table: Table, row: Row) extends PP_SQL_Table {
  def eval: Option[Table] =
    Try(Some(table.delete(row))).getOrElse(None)
}


/**
 * `implicit` -> conversia implicita a unei comenzi intr-un tabel
 *
 * @param t un tuplu cu 3 elemente:
 *            - tabelul din SQL
 *            - numele comenzii
 *            - parametrii comenzii DELETE (linia de sters dintr-un tabel
 * @return tabelul cu randul sters
 */
implicit def PP_SQL_Table_Delete(t: (Option[Table], String, Row)): Option[PP_SQL_Table] =
  t match {
    case (Some(table), "DELETE", row) => Some(DeleteRow(table, row))
    case _ => None
  }

case class FilterRows(table: Table, condition: FilterCond) extends PP_SQL_Table {
  def eval: Option[Table] =
    Try(Some(table.filter(condition))).getOrElse(None)
}

implicit def PP_SQL_Table_Filter(t: (Option[Table], String, FilterCond)): Option[PP_SQL_Table] =
  t match {
    case (Some(table), "FILTER", condition) => Some(FilterRows(table, condition))
    case _ => None
  }

// filtrarea tabelului
case class SelectColumns(table: Table, columns: List[String]) extends PP_SQL_Table {
  def eval: Option[Table] =
    Try(Some(table.select(columns))).getOrElse(None)
}

implicit def PP_SQL_Table_Select(t: (Option[Table], String, List[String])): Option[PP_SQL_Table] =
  t match {
    case (Some(table), "EXTRACT", columns) => Some(SelectColumns(table, columns))
    case _ => None
  }

def queryT(p: Option[PP_SQL_Table]): Option[Table] = p.flatMap(_.eval)
def queryDB(p: Option[PP_SQL_DB]): Option[Database] = p.flatMap(_.eval)
