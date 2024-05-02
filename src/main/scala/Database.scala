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

    if (t1.isEmpty || t2.isEmpty) return None

    val joinColumn1 = c1
    val joinColumn2 = c2

    val joinedData = t1.get.tableData.flatMap { row1 =>
      t2.get.tableData.filter { row2 =>
        row1(joinColumn1) == row2(joinColumn2) || (row1(joinColumn1) == "" && row2(joinColumn2) == "")
      }.map { row2 =>
        val joinedRow = row1 ++ row2.filterKeys(_ != joinColumn2)
          .map { case (k, v2) =>
            val v1 = row1.getOrElse(k, "")
            val newValue = if (v1.isEmpty) v2 else if (v1 != v2) s"$v1;$v2" else v1
            k -> newValue
          }
        joinedRow
      }
    }

    val additionalRowsTable1 = t1.get.tableData.filterNot(row1 =>
      joinedData.exists(joinedRow => joinedRow(joinColumn1) == row1(joinColumn1))
    ).map { row1 =>
      val newRow = row1 ++ t2.get.tableData.head.filterKeys(_ != joinColumn2)
        .map { case (k, _) => k -> row1.getOrElse(k, "") }
      newRow
    }

    val missingRows2 = t2.get.tableData.filterNot(row2 =>
      joinedData.exists(row => row(joinColumn1) == row2(joinColumn2))
    ).map { row2 =>
      val newRow = (t1.get.header.map(_ -> "").toMap + (c1 -> row2(c2))) ++ row2.filterKeys(_ != c2)
      newRow
    }

    val newTableData = joinedData ++ additionalRowsTable1 ++ missingRows2
    Some(Table(s"${table1}$table2", newTableData))
  }


  // implement indexing here
  def apply(i: Int): Table = {
    tables(i)
  }
}
