package de.bwhc.mtb.dto.extensions


import scala.util.{
  Try,
  Success
}
import de.bwhc.mtb.dtos.{
  Coding,
  ICD10GM,
  ICDO3T,
  ICDO3M,
  Medication,
  Gene
}
import de.bwhc.catalogs.med.{
  MedicationCatalog => ATCCatalog,
  Medication => ATCMedication
}
import de.bwhc.catalogs.icd.{
  ICD10GMCoding,
  ICD10GMCatalogs,
  ICD10GM => ICD10,
  ICDO3Catalogs,
}
import de.bwhc.catalogs.hgnc.{
  HGNCCatalog,
  HGNCGene,
  HGNCId
}


object CodingExtensions
{


  implicit class ICD10CodingOps(val coding: Coding[ICD10GM]) extends AnyVal
  {

    def complete(
      implicit catalogs: ICD10GMCatalogs
    ): Coding[ICD10GM] = {
      catalogs.coding(
        ICD10.Code(coding.code.value),
        coding.version.getOrElse(catalogs.latestVersion)
      )
      .map(
        icd10 =>
          coding.copy(
            display = Some(icd10.display),
            version = Some(icd10.version)
          )
      )
      .getOrElse(coding)
    }


    def superClass(
      implicit catalogs: ICD10GMCatalogs
    ): Option[Coding[ICD10GM]] = {
      catalogs.coding(
        ICD10.Code(coding.code.value),
        coding.version.getOrElse(catalogs.latestVersion)
      )
      .flatMap(
        icd10 => icd10.superClass.flatMap(catalogs.coding(_,icd10.version))
      )
      .map(
        superClass => 
          coding.copy(
            code = ICD10GM(superClass.code.value),
            display = Some(superClass.display)
          )
      )
    }


    def subClasses(
      implicit catalogs: ICD10GMCatalogs
    ): Set[Coding[ICD10GM]] = {
      catalogs.coding(
        ICD10.Code(coding.code.value),
        coding.version.getOrElse(catalogs.latestVersion)
      )
      .map(
        icd10 =>
          icd10.subClasses
            .flatMap(catalogs.coding(_,icd10.version))
            .map(
              subClass => 
                Coding(
                  ICD10GM(subClass.code.value),
                  Some(subClass.display),
                  Some(subClass.version)
              )
            )
      )
      .getOrElse(Set.empty)
    }

  }


  implicit class ICDO3TCodingOps(val coding: Coding[ICDO3T]) extends AnyVal
  {

    def complete(
      implicit catalogs: ICDO3Catalogs
    ): Coding[ICDO3T] = {
      (
        coding.version match {
          case None =>
            catalogs.topographyCodings()
              .find(_.code.value == coding.code.value)

          case Some(version) =>
            Try {
              catalogs.topographyCodings(version)
                .find(_.code.value == coding.code.value)
            }
            .getOrElse(None)
        }
      )
      .map(icdo3 => coding.copy(display = Some(icdo3.display)))
      .getOrElse(coding)
    }

  }


  implicit class ICDO3MCodingOps(val coding: Coding[ICDO3M]) extends AnyVal
  {

    def complete(
      implicit catalogs: ICDO3Catalogs
    ): Coding[ICDO3M] = {
      (
        coding.version match {
          case None =>
            catalogs.morphologyCodings()
              .find(_.code.value == coding.code.value)

          case Some(version) =>
            Try {
              catalogs.morphologyCodings(version)
                .find(_.code.value == coding.code.value)
            }
            .getOrElse(None)
        }
      )
      .map(icdo3 => coding.copy(display = Some(icdo3.display)))
      .getOrElse(coding)
    }

  }


  implicit class MedicationCodingOps(val coding: Medication.Coding) extends AnyVal
  {

    import Medication.System._

    def complete(
      implicit atcCatalogs: ATCCatalog
    ): Medication.Coding = 
      coding.system match {
        case ATC =>
          atcCatalogs
            .findWithCode(coding.code.value,coding.version.getOrElse(atcCatalogs.latestVersion))
            .map(
              atc =>
                coding.copy(
                  display = Some(atc.name),
                  version = Some(atc.version)
                )
            )
            .getOrElse(coding)

        case Unregistered => coding

      }  


    def medicationGroup(
      implicit atcCatalogs: ATCCatalog
    ): Option[Medication.Coding] = {

      coding.system match {
        case ATC =>
          atcCatalogs
            .findWithCode(coding.code.value,coding.version.getOrElse(atcCatalogs.latestVersion))
            .filter(
              _.kind == ATCMedication.Kind.Substance
            )
            .flatMap(
              atc => atc.parent.flatMap(atcCatalogs.find(_,atc.version))
            )
            .map(
              group =>
                coding.copy(
                  code = Medication.Code(group.code.value),
                  display = Some(group.name)
                )
            )

        case Unregistered => Some(coding)

      }  
    }  


    def childSubstances(
      implicit atcCatalogs: ATCCatalog
    ): Set[Medication.Coding] = {

      coding.system match {
        case ATC =>
          atcCatalogs
            .findWithCode(coding.code.value,coding.version.getOrElse(atcCatalogs.latestVersion))
            .filter(
              _.kind == ATCMedication.Kind.Group
            )
            .map(
              atc =>
                atc.children
                  .flatMap(atcCatalogs.find(_,atc.version))
                  .map(
                    substance =>
                      Medication.Coding(
                        Medication.Code(substance.code.value),
                        ATC,
                        Some(substance.name),
                        Some(substance.version)
                      )
                  )
            )
            .getOrElse(Set.empty)

        case Unregistered => Set.empty

      }  
    }

  }


  implicit class GeneCodingOps(val coding: Gene.Coding) extends AnyVal
  {

    def complete(
      implicit hgnc: HGNCCatalog[cats.Id]
    ): Gene.Coding = {

      import Gene._

      coding.hgncId.flatMap(
        id => hgnc.gene(HGNCId(id.value))
      )
      .orElse(
        coding.ensemblId.flatMap(
          id => hgnc.geneWithEnsemblId(id.value)
        )
      )
      .orElse(
        coding.symbol.flatMap(
          sym => hgnc.geneWithApprovedSymbol(sym.value)
        )
      )
      .map(
        gene =>
          coding.copy(
            gene.ensemblId.map(id => EnsemblId(id.value)),
            Some(HgncId(gene.hgncId.value)),
            Some(Symbol(gene.symbol)),
            Some(gene.name)
          )
      )
      .getOrElse(coding)

    }
  }


}
