#include <curl/curl.h>
#include <libxml2/libxml/xpath.h>
#include <stdio.h>
#include <stdlib.h>

#include "stopif.h"

char *rss_url = "https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml";
/**< The URL for an NYT RSS. */
char *rssfile = "nytimes_feeds.rss"; /**< A local file to write the RSS to.*/
char *outfile = "now.html"; /**< The output file to open in your browser.*/

void print_to_html(xmlXPathObjectPtr urls, xmlXPathObjectPtr titles) {
	FILE *f = fopen(outfile, "w");
	int i = 0;
	for (i = 0; i < titles->nodesetval->nodeNr; ++i) {
		fprintf(f, "<a href=\"%s\">%s</a><br>\n",
				xmlNodeGetContent(urls->nodesetval->nodeTab[i]),
				xmlNodeGetContent(titles->nodesetval->nodeTab[i]));
	}
	fclose(f);
}

int parse(char const *infile) {
	const xmlChar *titlepath = (xmlChar *)("//item/title");
	const xmlChar *linkpath = (xmlChar *)("//item/link");

	xmlDocPtr doc = xmlParseFile(infile);
	STOP_IF(!doc, return -1, "Error: unable to parse the file");

	xmlXPathContextPtr context = xmlXPathNewContext(doc);
	STOP_IF(!context, return -1, "Error: unable to create new XPath context\n");

	xmlXPathObjectPtr titles = xmlXPathEvalExpression(titlepath, context);
	xmlXPathObjectPtr urls = xmlXPathEvalExpression(linkpath, context);
	STOP_IF(!titles || !urls, return -3, "Failed to parse Xpath");

	print_to_html(urls, titles);

	xmlXPathFreeObject(titles);
	xmlXPathFreeObject(urls);
	xmlXPathFreeContext(context);
	xmlFreeDoc(doc);
	return 0;
}

int get_rss(char const *url, char const *outfile) {
	FILE *feedfile = fopen(outfile, "w");
	if (!feedfile) {
		return -1;
	}

	CURL *curl = curl_easy_init();
	if (!curl) {
		return -1;
	}

	curl_easy_setopt(curl, CURLOPT_URL, url);
	curl_easy_setopt(curl, CURLOPT_WRITEDATA, feedfile);
	// You need to check the documentation of curl_easy_perform... Return 0
	// means OK! The Result type of Rust is much better!
	CURLcode res = curl_easy_perform(curl);
	if (res) {
		return -1;
	}

	curl_easy_cleanup(curl);
	fclose(feedfile);
	return 0;
}

int main(int argc, char *argv[]) {
	STOP_IF(get_rss(rss_url, rssfile), return EXIT_FAILURE,
			"Failed to download %s to %s.\n", rss_url, rssfile);
	parse(rssfile);
	printf("Wrote headlines to %s. Have a look at it in the browser.\n",
		   outfile);
	return EXIT_SUCCESS;
}
