from loafer.items import LoaferItem
from scrapy import Spider, Request
import re

class LoaferSpider(Spider):
    name = 'loafer_spider'
    allowed_urls = ['https://www.zappos.com']
    start_urls = ['https://www.zappos.com/men-loafers/CK_XARC21wHAAQLiAgMBAhg.zso']

    def parse(self, response):
        page_range = response.xpath('//div[@class="Zh-z"]/text()').extract_first()
        groups = re.search('1 of (\d+)', page_range)
        num_pages = int(groups.group(1))
        result_urls = ['https://www.zappos.com/men-loafers/CK_XARC21wHAAQLiAgMBAhg.zso?p={}'.format(x) for x in range(num_pages)] 

        for url in result_urls:
            yield Request(url=url, callback=self.parse_results_page)

    def parse_results_page(self, response):
        product_urls = response.xpath('//article[@class="Xy-z Ql-z"]/a/@href').extract()
        product_urls = ['https://www.zappos.com' + url for url in product_urls]

        # print('='*55)
        # print(len(product_urls))
        # print('='*55)


        for url in product_urls:
            yield Request(url=url, callback=self.parse_product_page)

    def parse_product_page(self, response):
        
        try:
            brand = response.xpath('//span[@class="_N-z"]/a/span/text()').extract_first()
            model = response.xpath('//span[@class="aO-z"]/text()').extract_first()
        
        except Exception as e:
            print(type(e), e)
            brand = "None"
            model = "None"
        
        
        try:
            price = float(response.xpath('//div[@class="sN-z"]/span/text()').extract_first().split('$')[1])

        except Exception as e:
            print(type(e), e)
            price = 0
            

        try:

            fit = response.xpath('//div[@class="xl-z"]/strong/text()').extract()
            true_fit = int(fit[0])
            true_width = int(fit[2])
            mod_arc_s = int(fit[4])

        except Exception as e:
            print(type(e), e)
            true_fit = 0
            true_width = 0
            mod_arc_s = 0

            
        try:
            rating = float(response.xpath('//span[@class="jg-z cO-z"]/span[@class="kg-z"]/text()').extract_first())
            num_review = int(response.xpath('//span[@class="dO-z"]/text()').extract_first())

        except Exception as e:
            print(type(e), e)
            rating = 0.0
            num_review = 0 

        
        item = LoaferItem()
        item['brand'] = brand
        item['model'] = model
        item['price'] = price
        item['true_fit'] = true_fit
        item['true_width'] = true_width
        item['mod_arc_s'] = mod_arc_s
        item['rating'] = rating
        item['num_review'] = num_review

        yield item



