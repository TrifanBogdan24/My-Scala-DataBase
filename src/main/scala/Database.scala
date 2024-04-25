case class Database(tables: List[Table]) {
  override def toString: String = {
    tables.map(_.toString).mkString("\n\n")
  }

  def create(tableName: String): Database = {
    if (!tables.exists(_.tableName == tableName)) {
      val newTable = Table(tableName, List.empty)
      Database(tables :+ newTable)
    } else {
      this
    }
  }

  def drop(tableName: String): Database = {
    val updatedTables = tables.filterNot(_.tableName == tableName)
    Database(updatedTables)
  }

  def selectTables(tableNames: List[String]): Option[Database] = {
    val selectedTables = tableNames.flatMap(name => tables.find(_.tableName == name))
    if (selectedTables.length == tableNames.length) {
      Some(Database(selectedTables))
    } else {
      None
    }
  }

  def join(table1: String, c1: String, table2: String, c2: String): Option[Table] = {
    val t1 = tables.find(_.tableName == table1)
    val t2 = tables.find(_.tableName == table2)

    (t1, t2) match {
      case (Some(table1), Some(table2)) =>
        val joinedData = joinTables(table1.tableData, table2.tableData, c1, c2)
        Some(Table(s"${table1.tableName}_${table2.tableName}", joinedData))

      case _ => None
    }
  }

  // Funcție recursivă pentru join
  private def joinTables(table1Data: Tabular, table2Data: Tabular, c1: String, c2: String): Tabular = {
    def mergeRows(row1: Row, row2: Row): Row = {
      val commonKeys = row1.keys.toSet intersect row2.keys.toSet
      val newRow = (row1 ++ row2).map {
        case (k, v) if commonKeys.contains(k) => k -> v
        case (k, v) if k.endsWith("_2") => k -> v
        case (k, v) => k -> row1.getOrElse(k, "") // Use value from row1 if not present in row2
      }
      newRow
    }

    table1Data.flatMap { row1 =>
      val matchingRows = table2Data.filter(row2 => row1.getOrElse(c1, "") == row2.getOrElse(c2, ""))
      matchingRows.map(mergeRows(row1, _))
    }
  }

  // implement indexing here
  def apply(i: Int): Table = {
    tables(i)
  }
}
