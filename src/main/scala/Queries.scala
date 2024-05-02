object Queries {


  def killJackSparrow(t: Table): Option[Table] = {
    val condition: FilterCond = Field("name", name => name != "Jack")
    val filteredData = t.filter(condition)
    Some(filteredData)
  }

  def insertLinesThenSort(db: Database): Option[Table] = {
    // Creăm tabela "Inserted Fellas" și inserăm datele specificate
    val insertedTable = Table("Inserted Fellas", List(
      Map("name" -> "Ana", "age" -> "93", "CNP" -> "455550555"),
      Map("name" -> "Diana", "age" -> "33", "CNP" -> "255532142"),
      Map("name" -> "Tatiana", "age" -> "55", "CNP" -> "655532132"),
      Map("name" -> "Rosmaria", "age" -> "12", "CNP" -> "855532172")
    ))

    // Sortăm tabela după vârstă și returnăm rezultatul
    Some(insertedTable.sort("age"))
  }



  def youngAdultHobbiesJ(db: Database): Option[Table] =
    queryDB(Some(JoinTables(db, "People", "name", "Hobbies", "name")))
      .flatMap(db =>
        queryT(Some(FilterRows(Table("JoinedTables", db.tables.head.tableData),
          And(And(Field("age", _ < "25"), Field("name", _.startsWith("J"))),
            Field("hobby", _.nonEmpty)))))
          .flatMap(table =>
            queryT(Some(SelectColumns(table, List("name", "hobby"))))))

}
