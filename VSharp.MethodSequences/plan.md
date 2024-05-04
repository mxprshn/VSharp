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
- валится конструктор Func с IntPtr
- Исполнение RotateRadians(Double) не завершается

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

### Перепрогнать
! И выставить старый таймаут !

openra-OpenRA.Graphics.SheetBuilder-Add-100666807

osu-osu.Game.Rulesets.RulesetInfo-CompareTo-100678064
osu-osu.Game.Rulesets.RulesetInfo-CreateInstance-100678069
osu-osu.Game.Beatmaps.BeatmapInfo-AudioEquals-100698739
osu-osu.Game.Beatmaps.BeatmapInfo-BackgroundEquals-100698740
osu-osu.Game.Beatmaps.BeatmapInfo-ResetOnlineInfo-100698706

btcpayserver-BTCPayServer.Payments.Bitcoin.BitcoinLikePaymentData-PaymentCompleted-100670672
btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-GetInternalTags-100674121
btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-UpdateTotals-100674130
btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-GetPaymentMethods-100674204
btcpayserver-BTCPayServer.Services.Invoices.PaymentMethod-GetPaymentMethodDetails-100674279
btcpayserver-BTCPayServer.Data.StoreBlob-GetDefaultRateRules-100674934

osu-osu.Game.Screens.Select.BeatmapCarousel-SelectNext-100667140
osu-osu.Game.Screens.Select.BeatmapCarousel-SelectNextRandom-100667143
osu-osu.Game.Rulesets.Objects.HitObject-CreateSlidingSamples-100679061
osu-osu.Game.Rulesets.Objects.HitObject-CreateHitSampleInfo-100679062

osu-osu.Game.Screens.Select.BeatmapCarousel-SelectBeatmap-100667139
osu-osu.Game.Scoring.ScoreInfo-GetStatisticsForDisplay-100677790
osu-osu.Game.Overlays.Mods.ModPresetTooltip-SetContent-100687112

btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-UpdateTotals-100674130
btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-EntityToDTO-100674200
btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-GetPaymentMethods-100674204

osu-osu.Game.Overlays.Mods.ModColumn-SelectAll-100686952 
osu-osu.Game.Overlays.Mods.ModColumn-FlushPendingSelections-100686954

osu-osu.Game.Online.Chat.Channel-AddLocalEcho-100691559
osu-osu.Game.Online.Chat.Channel-ReplaceMessage-100691562

## Выкинуть

openra-add
osu-osu.Game.Rulesets.Objects.HitObject-CreateSlidingSamples-100679061 -- генерация сиквенса не завершилась
RotateRadians/Degrees

## Replays

- Неправильно replay-ятся тесты для openra-OpenRA.Graphics.SheetBuilder-Add-100666807
