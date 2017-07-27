import com.softwaremill.sttp._
import com.typesafe.config.ConfigFactory

object StatComma {

  object Configuration {
    private val configuration = ConfigFactory.load()

    def apiKey: String = configuration.getString("apiKey")
  }

  object Tags {
    def Domain = "Domain"
    def MainDomain = "MainDomain"
    def Name = "Name"
    def RecordType = "RecordType"
    def SubDomainRecord = "SubDomainRecord"
    def SubHost = "Subhost"
    def Value = "Value"
  }

  object Commands {
    def ListDomains = "list_domain"
  }

  case class MainDomainRecord(recordType: String, value: String)

  case class SubDomainRecord(recordType: String, value: String, subHost: String)

  case class DomainRecord(name: String, mainDomain: MainDomainRecord, subDomains: Seq[SubDomainRecord]) {
    override def toString: String = s"$name:\nDomain Record: $mainDomain\nSubdomain Records: ${subDomains}"
  }

  def baseURL = "https://api.dynadot.com/api3.xml"

  def performAuthenticatedRequest(queryParams: Map[String, String]): String = {
    val apiKey = Configuration.apiKey
    val paramsWithAuth = queryParams.updated("key", apiKey)
    implicit val handler = HttpURLConnectionSttpHandler
    sttp.get(uri"$baseURL?$paramsWithAuth").send().body
  }

  def runCommand(command: String, params: Map[String, String] = Map()): xml.NodeSeq = {
    val paramsWithCommand = params.updated("command", command)
    val response = performAuthenticatedRequest(paramsWithCommand)
    xml.XML.loadString(response)
  }

  def getTagText(node: xml.NodeSeq, tag: String): String = {
    val tagNode = node \\ tag
    tagNode.head.text
  }

  def getMainDomainRecord(domain: xml.Node): MainDomainRecord = {
    val mainDomainXML = domain \\ Tags.MainDomain
    val recordType = getTagText(mainDomainXML, Tags.RecordType)
    val value = getTagText(mainDomainXML, Tags.Value)
    MainDomainRecord(recordType, value)
  }

  def getSubDomainRecord(subDomain: xml.Node): SubDomainRecord = {
    List(Tags.SubHost, Tags.RecordType, Tags.Value).map(tag => getTagText(subDomain, tag)) match {
      case List(subHost, recordType, value) =>
        SubDomainRecord(subHost = subHost, recordType = recordType, value = value)
    }
  }

  def getDomainName(domainXML: xml.NodeSeq): String = {
    getTagText(domainXML, Tags.Name)
  }

  def getSubDomainRecords(domainXML: xml.NodeSeq): Seq[SubDomainRecord] = {
    val subDomains = domainXML \\ Tags.SubDomainRecord
    subDomains.map(getSubDomainRecord)
  }

  def getDomainXMLs(domainListXML: xml.NodeSeq) = {
    domainListXML \\ Tags.Domain
  }

  def getDomainRecord(domainXml: xml.Node): DomainRecord = {
    val name = getDomainName(domainXml)
    val mainDomain = getMainDomainRecord(domainXml)
    val subDomains = getSubDomainRecords(domainXml)
    DomainRecord(name, mainDomain, subDomains)
  }

  def listDomains: Seq[DomainRecord] = {
    val domainListXML = runCommand(Commands.ListDomains)
    val domainXMLs = getDomainXMLs(domainListXML)
    domainXMLs.map(getDomainRecord)
  }

}
