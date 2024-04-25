object Queries {

  def killJackSparrow(t: Table): Option[Table] = {
    queryT(Some(FilterRows(t, Field("name", _ != "Jack"))))
  }

  def insertLinesThenSort(db: Database): Option[Table] = {
    ???
  }
  def youngAdultHobbiesJ(db: Database): Option[Table] = {
    ???
  }
}
