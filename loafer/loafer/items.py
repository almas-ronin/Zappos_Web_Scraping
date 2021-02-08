# Define here the models for your scraped items
#
# See documentation in:
# https://docs.scrapy.org/en/latest/topics/items.html

import scrapy


class LoaferItem(scrapy.Item):
    # define the fields for your item here like:
    # name = scrapy.Field()
    brand = scrapy.Field()
    model = scrapy.Field()
    price = scrapy.Field()
    true_fit = scrapy.Field()
    true_width = scrapy.Field()
    mod_arc_s = scrapy.Field()
    num_review = scrapy.Field() 
    rating = scrapy.Field()
