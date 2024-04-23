#!/bin/bash

bench_ids=(
  "btcpayserver-BTCPayServer.DerivationSchemeParser-ParseOutputDescriptor-100669018"
  "btcpayserver-BTCPayServer.DerivationSchemeParser-ParseElectrum-100669019"
  "btcpayserver-BTCPayServer.DerivationSchemeParser-Parse-100669020"
  "btcpayserver-BTCPayServer.Payments.LNURLPayPaymentMethodDetails-GetAdditionalDataPartialName-100670327"
  "btcpayserver-BTCPayServer.Payments.LNURLPayPaymentMethodDetails-GetAdditionalData-100670328"
  "btcpayserver-BTCPayServer.Payments.Lightning.LightningLikePaymentData-GetPaymentType-100670479"
  "btcpayserver-BTCPayServer.Payments.Lightning.LightningLikePaymentData-GetPaymentId-100670478"
  "btcpayserver-BTCPayServer.Payments.Lightning.LightningSupportedPaymentMethod-GetExternalLightningUrl-100670594"
  "btcpayserver-BTCPayServer.Payments.Lightning.LightningSupportedPaymentMethod-SetLightningUrl-100670595"
  "btcpayserver-BTCPayServer.Payments.Lightning.LightningSupportedPaymentMethod-GetDisplayableConnectionString-100670596"
  "btcpayserver-BTCPayServer.Payments.Bitcoin.BitcoinLikePaymentData-PaymentConfirmed-100670673"
  "btcpayserver-BTCPayServer.Payments.Bitcoin.BitcoinLikePaymentData-PaymentCompleted-100670672"
  "btcpayserver-BTCPayServer.Models.StoreViewModels.DerivationSchemeViewModel-GetAccountKeypath-100671449"
  "btcpayserver-BTCPayServer.HostedServices.BackgroundJobClient-ProcessJobs-100672745"
  "btcpayserver-BTCPayServer.HostedServices.BackgroundJobClient-WaitAllRunning-100672744"
  "btcpayserver-BTCPayServer.HostedServices.CheckConfigurationHostedService-StopAsync-100672769"
  "btcpayserver-BTCPayServer.HostedServices.CheckConfigurationHostedService-StartAsync-100672767"
  "btcpayserver-BTCPayServer.HostedServices.EventHostedServiceBase-ProcessEvents-100672800"
  "btcpayserver-BTCPayServer.HostedServices.EventHostedServiceBase-StartAsync-100672805"
  "btcpayserver-BTCPayServer.HostedServices.EventHostedServiceBase-StopAsync-100672806"
  "btcpayserver-BTCPayServer.Services.DynamicDnsService-SendUpdateRequest-100673313"
  "btcpayserver-BTCPayServer.Services.DynamicDnsService-CreateUpdateRequest-100673314"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-GetInternalTags-100674121"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-UpdateTotals-100674130"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-EntityToDTO-100674200"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-GetPaymentMethods-100674204"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-SetPaymentMethods-100674206"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-IsUnsetTopUp-100674209"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceEntity-GetSupportedPaymentMethod-100674126"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceState-CanMarkComplete-100674223"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceState-CanMarkInvalid-100674224"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceState-CanRefund-100674225"
  "btcpayserver-BTCPayServer.Services.Invoices.InvoiceState-IsSettled-100674226"
  "btcpayserver-BTCPayServer.Services.Invoices.PaymentMethodAccounting-ToSmallestUnit-100674256"
  "btcpayserver-BTCPayServer.Services.Invoices.PaymentMethod-GetPaymentMethodDetails-100674279"
  "btcpayserver-BTCPayServer.Services.Invoices.PaymentMethod-Calculate-100674287"
  "btcpayserver-BTCPayServer.Services.Invoices.PaymentEntity-UpdateAmounts-100674328"
  "btcpayserver-BTCPayServer.Services.Invoices.PaymentEntity-GetCryptoPaymentData-100674329"
  "btcpayserver-BTCPayServer.Services.Invoices.PaymentEntity-GetPaymentMethodId-100674331"
  "btcpayserver-BTCPayServer.Data.StoreBlob-SetExcluded-100674966"
  "btcpayserver-BTCPayServer.Data.StoreBlob-IsExcluded-100674965"
  "btcpayserver-BTCPayServer.Data.StoreBlob-GetExcludedPaymentMethods-100674964"
  "btcpayserver-BTCPayServer.Data.StoreBlob-GetDefaultRateRules-100674934"
  "btcpayserver-BTCPayServer.Data.WalletTransactionInfo-Merge-100674998"
  "vsharp-IntegrationTests.LoanExam-Build-100664240"

)

timestamp=$(date +"%Y-%m-%d_%H-%M-%S")
critical_failures_log="/Users/max/Repos/MethodSequencesBenches/Runs/criticals_$timestamp.log"
cat critical_failures_log

for bench_id in "${bench_ids[@]}"; do
    echo "Running $bench_id..."
    /Users/max/Repos/VSharp/VSharp.MethodSequencesBenchmarks/bin/Debug/net7.0/VSharp.MethodSequencesBenchmarks run "$bench_id" "/Users/max/Repos/MethodSequencesBenches/Benches" -o "/Users/max/Repos/MethodSequencesBenches/Runs" 2>> "$critical_failures_log"
done
