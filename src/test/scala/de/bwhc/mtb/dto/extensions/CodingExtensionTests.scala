package de.bwhc.mtb.dto.extensions



import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers._
import org.scalatest.Inspectors._
import org.scalatest.OptionValues._
import de.bwhc.catalogs.med.MedicationCatalog
import de.bwhc.catalogs.icd.ICD10GMCatalogs
import de.bwhc.catalogs.hgnc.HGNCCatalog
import de.bwhc.mtb.dtos.{
  Coding,
  ICD10GM,
  Medication,
  Gene
}



class CodingExtensionTests extends AnyFlatSpec
{

  import CodingExtensions._


  implicit val icd10gm = ICD10GMCatalogs.getInstance.get
  implicit val atc     = MedicationCatalog.getInstance.get
  implicit val hgnc    = HGNCCatalog.getInstance.get


  val c25 =
    Coding(
      ICD10GM("C25"),
      None,
      None
    )

  val c25_1 =
    Coding(
      ICD10GM("C25.1"),
      None,
      None
    )


  "Coding[ICD10GM]" must "have been completed" in {

    val coding = c25.complete

    coding.display mustBe defined  
    coding.version mustBe defined  

  }


  "Coding[ICD10GM] subClasses" must "have been correctly resolved" in {

    val subClasses = c25.subClasses

    subClasses must not be empty

    forAll(subClasses){ _.code.value must startWith ("C25") }


    c25_1.subClasses mustBe empty

  }


  "Coding[ICD10GM] superClass" must "have been correctly resolved" in {

    val superClass = c25_1.superClass

    superClass.value.code.value must equal ("C25")


    c25.superClass must not be defined
  }


  val mTOR =
    Medication.Coding(
      Medication.Code("L01EG"),
      Medication.System.ATC,
      None,
      None
    )

  val everolimus =
    Medication.Coding(
      Medication.Code("L01EG02"),
      Medication.System.ATC,
      None,
      None
    )


  "Medication.Coding" must "have been completed" in {

     val coding = everolimus.complete

     coding.display mustBe defined  
     coding.version mustBe defined  

  }


  "Medication.Coding substances" must "have been correctly resolved" in {
    
    val substances = mTOR.childSubstances

    substances must not be empty

    forAll(substances){ _.code.value must startWith (mTOR.code.value) }
  }


  "Medication.Coding group" must "have been correctly resolved" in {

    val group = everolimus.medicationGroup

    group.value.code must equal (mTOR.code)

  }




  val tp53 =
    Gene.Coding(
      None,
      None,
      Some(Gene.Symbol("TP53")),
      None,
    )

  val hgnc22 =
    Gene.Coding(
      None,
      Some(Gene.HgncId("HGNC:22")),
      None,
      None,
    )

  val genes =
    List(
      tp53,
      hgnc22
    )


  "Gene.Codings" must "have been completed" in {

    forAll(
      genes.map(_.complete)
    ){
      coding =>
        coding.hgncId mustBe defined
        coding.name mustBe defined
    }
  }

}
