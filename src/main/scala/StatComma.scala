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
    def SubDomainRecord = "SubDomainRecord"
    def RecordType = "RecordType"
    def Value = "Value"
    def SubHost = "Subhost"
  }

  object Commands {
    def ListDomains = "list_domain"
  }

  case class DomainRecord(recordType: String, value: String)

  case class SubDomainRecord(subHost: String, recordType: String, value: String)

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

  def getMainDomainRecord(domain: xml.Node): DomainRecord = {
    val mainDomainXML = domain \\ Tags.MainDomain
    val recordType = getTagText(mainDomainXML, Tags.RecordType)
    val value = getTagText(mainDomainXML, Tags.Value)
    DomainRecord(recordType, value)
  }

  def getSubDomainRecord(subDomain: xml.Node): SubDomainRecord = {
    List(Tags.SubHost, Tags.RecordType, Tags.Value).map(tag => getTagText(subDomain, tag)) match {
      case List(subHost, recordType, value) =>
        SubDomainRecord(subHost = subHost, recordType = recordType, value = value)
    }
  }

  def getSubDomainRecords(domainXML: xml.NodeSeq): Seq[SubDomainRecord] = {
    val subDomains = domainXML \\ Tags.SubDomainRecord
    println(subDomains.size)
    subDomains.map(getSubDomainRecord)
  }

  def getDomainRecord(domainXml: xml.Node): (DomainRecord, Seq[SubDomainRecord]) = {
    val mainDomain = getMainDomainRecord(domainXml)
    val subDomains = getSubDomainRecords(domainXml)
    (mainDomain, subDomains)
  }

  def listDomains: Seq[(DomainRecord, Seq[SubDomainRecord])] = {
    val domainListXML = runCommand(Commands.ListDomains)
    domainListXML.map(getDomainRecord)
  }

}
