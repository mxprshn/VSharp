using System.Collections;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using VSharp.CSharpUtils;

namespace VSharp.Test.Benchmarks;

public class BtcPayServerTargets : BenchmarkTargets
{
    public BtcPayServerTargets(TextWriter logWriter, string benchmarksPath) :
        base(logWriter, benchmarksPath, "btcpayserver", "BTCPayServer")
    {
    }

    public IEnumerable<BenchmarkTarget> DerivationSchemeParser() =>
        TargetsForMethods("DerivationSchemeParser",
            "ParseOutputDescriptor", "ParseElectrum", "Parse");

    public IEnumerable<BenchmarkTarget> LNURLPayPaymentMethodDetails() =>
        TargetsForMethods("LNURLPayPaymentMethodDetails",
            "GetAdditionalDataPartialName", "GetAdditionalData");

    public IEnumerable<BenchmarkTarget> LightningLikePaymentData() =>
        TargetsForMethods("LightningLikePaymentData",
            "GetPaymentType", "GetPaymentId");

    public IEnumerable<BenchmarkTarget> LightningSupportedPaymentMethod() =>
        TargetsForMethods("LightningSupportedPaymentMethod",
            "GetExternalLightningUrl",
            "SetLightningUrl",
            "GetDisplayableConnectionString");

    public IEnumerable<BenchmarkTarget> BitcoinLikePaymentData() =>
        TargetsForMethods("BitcoinLikePaymentData", "PaymentConfirmed", "PaymentCompleted");

    public IEnumerable<BenchmarkTarget> DerivationSchemeViewModel() =>
        TargetsForMethods("DerivationSchemeViewModel", "GetAccountKeypath");

    public IEnumerable<BenchmarkTarget> BackgroundJobClient() =>
        TargetsForMethods("BackgroundJobClient", "ProcessJobs", "WaitAllRunning");

    public IEnumerable<BenchmarkTarget> CheckConfigurationHostedService() =>
        TargetsForMethods("CheckConfigurationHostedService", "StopAsync", "StartAsync");

    public IEnumerable<BenchmarkTarget> EventHostedServiceBase() =>
        TargetsForMethods("EventHostedServiceBase", "ProcessEvents", "StartAsync", "StopAsync");

    public IEnumerable<BenchmarkTarget> DynamicDnsService() =>
        TargetsForMethods("DynamicDnsService", "SendUpdateRequest", "CreateUpdateRequest");

    public IEnumerable<BenchmarkTarget> InvoiceEntity() =>
        TargetsForMethods("InvoiceEntity", "GetInternalTags", "UpdateTotals", "EntityToDTO",
            "GetPaymentMethods", "SetPaymentMethods", "IsUnsetTopUp", "100674126", "GetInternalTags");

    public IEnumerable<BenchmarkTarget> InvoiceState() =>
        TargetsForMethods("InvoiceState", "CanMarkComplete", "CanMarkInvalid",
            "CanRefund", "IsSettled");

    public IEnumerable<BenchmarkTarget> PaymentMethodAccounting() =>
        TargetsForMethods("PaymentMethodAccounting", "ToSmallestUnit");

    public IEnumerable<BenchmarkTarget> PaymentMethod() =>
        TargetsForMethods("PaymentMethod", "GetPaymentMethodDetails", "Calculate");

    public IEnumerable<BenchmarkTarget> PaymentEntity() =>
        TargetsForMethods("PaymentEntity", "UpdateAmounts", "GetCryptoPaymentData",
            "GetPaymentMethodId");

    // + GetRateRules(BTCPayNetworkProvider networkProvider, out bool preferredSource)
    public IEnumerable<BenchmarkTarget> StoreBlob() =>
        TargetsForMethods("StoreBlob", "SetExcluded", "IsExcluded", "GetExcludedPaymentMethods",
            "GetDefaultRateRules");

    public IEnumerable<BenchmarkTarget> WalletTransactionInfoMethods() =>
        TargetsForMethods("WalletTransactionInfo", "Merge");

    public override IEnumerable<BenchmarkTarget> All()
    {
        return DerivationSchemeParser()
            .Concat(LNURLPayPaymentMethodDetails())
            .Concat(LightningLikePaymentData())
            .Concat(LightningSupportedPaymentMethod())
            .Concat(BitcoinLikePaymentData())
            .Concat(DerivationSchemeViewModel())
            .Concat(BackgroundJobClient())
            .Concat(CheckConfigurationHostedService())
            .Concat(EventHostedServiceBase())
            .Concat(DynamicDnsService())
            .Concat(InvoiceEntity())
            .Concat(InvoiceState())
            .Concat(PaymentMethodAccounting())
            .Concat(PaymentMethod())
            .Concat(PaymentEntity())
            .Concat(StoreBlob())
            .Concat(WalletTransactionInfoMethods());
    }
}
