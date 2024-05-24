import scala.util.Try


object Queries {

  def killJackSparrow(t: Table): Option[Table] =
    queryT(Some(FilterRows(t, Field("name", _ != "Jack"))))

  def insertLinesThenSort(db: Database): Option[Table] = InsertRow(Table("Inserted Fellas", List.empty), List(
    Map("name" -> "Ana", "age" -> "93", "CNP" -> "455550555"),
    Map("name" -> "Diana", "age" -> "33", "CNP" -> "255532142"),
    Map("name" -> "Tatiana", "age" -> "55", "CNP" -> "655532132"),
    Map("name" -> "Rosmaria", "age" -> "12", "CNP" -> "855532172")
  )).eval.map(_.sort("age"))



  def youngAdultHobbiesJ(db: Database): Option[Table] =
    db.tables.find(_.tableName == "People")         // SELECT
      .flatMap(peopleTable =>
        db.tables.find(_.tableName == "Hobbies")    // JOIN
          .map(hobbiesTable =>
            Table("YoungAdultHobbies", peopleTable.tableData                                  // CREATE
              .filter(row => row("name").startsWith("J") && row("age").toInt < 25)            // FILTER
              .flatMap(person =>
                hobbiesTable.tableData.filter(_("name") == person("name"))
                  .map(hobby => Map("name" -> person("name"), "hobby" -> hobby("hobby")))     // FILTER
              )
            )
          )
      )


}
