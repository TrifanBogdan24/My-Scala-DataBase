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

    val data1 = t1.get.tableData
    val data2 = t2.get.tableData

    val joinColumn1 = c1
    val joinColumn2 = c2

    val joinedData = for {
      row1 <- data1
      row2 <- data2
      if row1(joinColumn1) == row2(joinColumn2) || (row1(joinColumn1) == "" && row2(joinColumn2) == "")
    } yield {
      val joinedRow = row1 ++ row2.filterKeys(_ != joinColumn2)
        .map { case (k, v2) =>
          val v1 = row1.getOrElse(k, "")
          val newValue = if (v1.isEmpty) v2 else if (v1 != v2) s"$v1;$v2" else v1
          k -> newValue
        }
      joinedRow
    }

    val additionalRowsTable1 = data1.filterNot(row1 => joinedData.exists(joinedRow => joinedRow(joinColumn1) == row1(joinColumn1)))
      .map(row1 => {
        val newRow = row1 ++ data2.head.filterKeys(_ != joinColumn2).map { case (k, _) => k -> row1.getOrElse(k, "") }
        newRow
      })


    val optTable1 = tables.find(_.name == table1)
    val optTable2 = tables.find(_.name == table2)

    (optTable1, optTable2) match {
      case (Some(t1), Some(t2)) =>
        // Extragem coloanele specificate pentru join
        val col1 = t1.header.indexOf(c1)
        val col2 = t2.header.indexOf(c2)

        // Realizăm unirea rândurilor din tabele
        val joinedData1 = for {
          row1 <- t1.data
          row2 <- t2.data
          if row1(c1) == row2(c2) || (row1(c1).isEmpty && row2(c2).isEmpty)
        } yield {
          // Creăm un nou Map care să conțină toate perechile cheie-valoare din ambele rânduri
          val newRow = row1 ++ row2

          // Actualizăm valoarea pentru cheia c1 conform condițiilor cerute
          val updatedValue = if (row1(c1).isEmpty) row2(c2) else row1(c1)
          val updatedRow = newRow.updated(c1, updatedValue)

          updatedRow
        }

        val data1 = optTable1.get.tableData
        val data2 = optTable2.get.tableData


        val missingRows2 = for {
          row2 <- t2.data
          if !joinedData1.exists(row => row(c2) == row2(c2))
        } yield {
          // Eliminăm adăugarea coloanei "person_name" dacă deja există în tabelul de rezultate
          val newRow = (t1.header.map(_ -> "").toMap + (c1 -> row2(c2))) ++ row2.filterKeys(_ != c2).filterKeys(_ != c2)
          newRow
        }

        val newTableData = joinedData ++ additionalRowsTable1 ++ missingRows2
        val newTable = Table(s"${table1}$table2", newTableData.map(_.toMap))
        Some(newTable)
    }
  }

  // implement indexing here
  def apply(i: Int): Table = {
    tables(i)
  }
}
