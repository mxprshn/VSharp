## Идеи

- Определить, какие аргументы участвуют в условии. Не добавлять ничего для тех аргументов, которые не участвуют
- Отделение независимых подмножеств аргументов. Нет смысла перебирать все комбинации (?)
- Использовать LLM, чтобы получить множество методов для перебора из сгенерированных тестов (+ количество вызовов)


## TODO

- ~~null-case~~
- Выставление паблик-полей
- Поиск для множества состояний одновременно с символьным исполнением (in progress)
- Поддержка не только конструкторов и свойств
- Моки
  - Правда ли, что мок == просто дополнительные аргументы
- byrefs

## Проблемы

- wlp валится на моках вшарпа

## Бенчмарки

### Простейшие

btcpayserver

- DerivationSchemeParser (все методы подходят)
- LNURLPayPaymentMethodDetails
- LightningLikePaymentData (можно GetPaymentType())
- LightningLikePaymentMethodDetails
- LightningSupportedPaymentMethod
- BitcoinLikeOnChainPaymentMethod
- BitcoinLikePaymentData
- DerivationSchemeViewModel
- BackgroundJobClient
- CheckConfigurationHostedService
- EventHostedServiceBase
- FieldValueMirror (нужны коллекции для Form)
- DynamicDnsService
- NBXSyncSummaryProvider
- InvoiceEntity.UpdateTotals() + там дохуя методов вообще
- InvoiceState
- PaymentMethodAccounting.ToSmallestUnit
- PaymentMethod
- PaymentEntity
- PaymentMethodDictionary
- FixedFeeProvider
- InvoiceDataChangedEvent
- StoreBlob
- WalletTransactionInfo
- AuthorizedWebhookEvents
- WebhookDeliveryBlob
- GreenfieldHealthController

litedb

- SharedEngine
- BsonArray (интересные кейсы, только если будут не только конструкторы)
- BsonDocument (так же)
- BsonValue (много конструкторов с разными примитивными типами)
- ObjectId (но есть статический конструктор)
- Collation
- LiteEngine


osu
- BeatmapCarousel (под вопросом, хз насколько конфигурируется сеттерами, но есть даже паблик поля)
- ScoreInfo -- может полезть в realm
- RulesetInfo.CreateInstance()
- HitObject
- PathType -- тут есть свойства с модификатором init
- DefaultJudgementPiece -- не факт, что сработает, возможно полезет в графику
- JudgementResult.TimeOffset
- ModColumn
- ModPresetTooltip.SetContent
- Channel
- SoloScoreInfo -- есть простейшие методы, сравнивающие чисто одно свойство
- BeatmapInfo
  
openra

- HotKey
- CellRegion
- MPos
- Order
- float2
- int2
- WAngle.Lerp
- WRot
- WVec
- Rectangle
- Sheet
- SheetBuilder

### Какую инфу сохранить?

Для каждой последовательости

- Время генерации
- Количество шагов
- Длина последовательности

Для каждого метода

- Время генерации
- Покрытие по всем состояниям
- Количество исключений
- Количество тестов
- Количество ошибок
