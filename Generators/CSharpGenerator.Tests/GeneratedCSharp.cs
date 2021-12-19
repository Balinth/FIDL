using System;

namespace CloudAPI
{
    public abstract sealed class Error
    {
        private readonly Error error;

        public sealed class AuthorizationError : Error
        {
            public string RequiredClaim { get; set; }
        }

        public sealed class IdMustBeUnique : Error
        {

        }

        public sealed class NotEnoughStorage : Error
        {
            public int MaxStoredBytes { get; set; }
            public int CurrentlyStoredBytes { get; set; }
        }

        public TResult Match<TResult>(
            Func<AuthorizationError, TResult> authErrorHandler,
            Func<IdMustBeUnique, TResult> idMustBeUniqueHandler,
            Func<NotEnoughStorage,TResult> notEnoughStorageHandler)
        {
            switch (error)
            {
                case AuthorizationError authError: return authErrorHandler(authError);
                case IdMustBeUnique idMustBeUnique: return idMustBeUniqueHandler(idMustBeUnique);
                case NotEnoughStorage notEnoughStorage: return notEnoughStorageHandler(notEnoughStorage);
                default: throw new ApplicationException($"Discriminated union object of type {nameof(Error)} was not one of the possible cases! Actual type of its value: {error.GetType().FullName}");
            }
        }
    }
}